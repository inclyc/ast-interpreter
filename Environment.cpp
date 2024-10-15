#include "Environment.h"
#include "ExprValue.h"
#include "StackFrame.h"
#include "Support.h"

#include <cassert>
#include <iostream>

using namespace clang;

Environment::Environment()
    : mStack(), mFree(nullptr), mMalloc(nullptr), mInput(nullptr),
      mOutput(nullptr), mEntry(nullptr) {}

void Environment::arraySubscript(const clang::ArraySubscriptExpr &asub) {
  const auto *idxExpr = asub.getIdx();
  const auto *baseExpr = asub.getBase();

  const auto idx = getStmtVal(assertDeref(idxExpr));

  // Assume base is "implict cast" + "decl ref"
  const auto *baseImplicitCast =
      llvm::dyn_cast_or_null<ImplicitCastExpr>(baseExpr);
  assert(baseImplicitCast && "non-trivial array subscript!");

  const auto *baseDeclRef =
      llvm::dyn_cast_or_null<DeclRefExpr>(baseImplicitCast->getSubExpr());

  assert(baseDeclRef && "non-trivial array subscript!");

  const auto obj = mStack.back().getStmt(baseDeclRef);

  // Subscripted array object is based on "base" obj
  // base + offset
  const auto objKind = obj.getKind();
  assert(objKind == ExprObject::ValueKind::REF_STACK && "array not on stack?");

  mStack.back().insertStmt(
      &asub, ExprObject::mkRefStack(obj.getData().mStackIndex + idx));
};

void Environment::integerLiteral(const IntegerLiteral &literal) {
  llvm::APInt value = literal.getValue();
  assert(value.getBitWidth() <= 32);
  mStack.back().insertStmt(&literal, ExprObject::mkVal(value.getZExtValue()));
}

namespace {

std::size_t calcSizeOf(const clang::Type &type) {
  return type.isIntegerType() ? sizeof(ValueTy) : sizeof(void *);
}

} // namespace

void Environment::unaryExprOrTypeTrait(
    const clang::UnaryExprOrTypeTraitExpr &ute) {
  const auto kind = ute.getKind();
  const auto &type = *ute.getArgumentType();
  assert(kind == clang::UETT_SizeOf && "Can only handle sizeof!");

  mStack.back().insertStmt(&ute, ExprObject::mkVal(calcSizeOf(type)));
}

void Environment::init(const TranslationUnitDecl &unit) {
  for (TranslationUnitDecl::decl_iterator i = unit.decls_begin(),
                                          e = unit.decls_end();
       i != e; ++i) {
    if (FunctionDecl *fdecl = dyn_cast<FunctionDecl>(*i)) {
      if (fdecl->getName().equals("FREE")) {
        mFree = fdecl;
      } else if (fdecl->getName().equals("MALLOC")) {
        mMalloc = fdecl;
      } else if (fdecl->getName().equals("GET")) {
        mInput = fdecl;
      } else if (fdecl->getName().equals("PRINT")) {
        mOutput = fdecl;
      } else if (fdecl->getName().equals("main")) {
        mEntry = fdecl;
      }
    }
  }
  mStack.push_back(StackFrame());
}

const clang::FunctionDecl *Environment::getEntry() const { return mEntry; }

void Environment::binop(const BinaryOperator &bop) {
  const auto *left = bop.getLHS();
  const auto *right = bop.getRHS();

  assert(left && right);

  if (bop.isAssignmentOp()) {
    const auto val = getStmtVal(*right);
    const auto leftObj = mStack.back().getStmt(left);
    auto &refLeft = refExpr(leftObj);
    refLeft = val;

    // Propogate the lvalue to binary op itself.
    // Thus we can support continous assignments.
    mStack.back().insertStmt(&bop, leftObj);
  } else {
    const auto lhs = getStmtVal(*left);
    const auto rhs = getStmtVal(*right);

    const auto val = [&]() -> ValueTy {
      // BinaryOP : + | - | * | / | < | > | ==
      switch (bop.getOpcode()) {
      case clang::BO_Add:
        return lhs + rhs;
      case clang::BO_Sub:
        return lhs - rhs;
      case clang::BO_Mul:
        return lhs * rhs;
      case clang::BO_Div:
        return lhs / rhs;
      case clang::BO_LT:
        return lhs < rhs;
      case clang::BO_GT:
        return lhs > rhs;
      case clang::BO_EQ:
        return lhs == rhs;
      default:
        assert(false && "unexpected binary operation!");
      }
    }();

    // The binary op evaluates to rvalue "val"
    mStack.back().insertStmt(&bop, ExprObject::mkVal(val));
  }
}

void Environment::paren(const ParenExpr &paren) {
  const auto *subexpr = paren.getSubExpr();
  const auto subval = getStmtVal(assertDeref(paren.getSubExpr()));
  mStack.back().insertStmt(&paren, mStack.back().getStmt(subexpr));
}

void Environment::unaryOp(const UnaryOperator &unaryOp) {
  const auto subval = getStmtVal(assertDeref(unaryOp.getSubExpr()));
  const auto val = [&]() {
    switch (unaryOp.getOpcode()) {
    case clang::UO_Minus:
      return ExprObject::mkVal(-subval);
    case clang::UO_Deref:
      return ExprObject::mkRefHeap(subval);
    default:
      assert(false && "unreachable!");
    }
  }();

  mStack.back().insertStmt(&unaryOp, val);
}

namespace {

std::size_t getSizeNeededForType(const clang::Type &type) {
  if (type.isConstantArrayType()) {
    const auto &arrayType = cast<ConstantArrayType>(type);
    return arrayType.getSize().getZExtValue();
  }
  return 1;
}

} // namespace

void Environment::decl(const DeclStmt &declstmt) {
  for (const auto decl : declstmt.decls()) {
    if (VarDecl *vardecl = dyn_cast<VarDecl>(decl)) {
      const auto *init = vardecl->getInit();
      const auto initValue = init ? getStmtVal(*init) : ValueTy(0);

      // Inspect the type of this var decl, it might be an array,
      const auto *type = vardecl->getType().getTypePtr();
      // Calculate how many size needed for this type.
      const auto size = getSizeNeededForType(assertDeref(type));

      mStack.back().allocDecl(decl, initValue, size);
    }
  }
}
void Environment::declref(const DeclRefExpr &declref) {
  mStack.back().setPC(&declref);
  const auto &type = *declref.getType();
  if (type.isIntegerType() || type.isConstantArrayType() ||
      type.isPointerType()) {
    const auto *decl = declref.getFoundDecl();
    assert(decl);

    if (mStack.back().containsDecl(decl)) {
      // Local variable.
      auto idx = mStack.back().getDeclIdx(decl);
      // Reference stack element at "idx".
      mStack.back().insertStmt(&declref, ExprObject::mkRefStack(idx));
    } else {
      // This must be a reference to global varaible.
      assert(mGlobalFrame.containsDecl(decl));
      auto idx = mGlobalFrame.getDeclIdx(decl);
      mStack.back().insertStmt(&declref, ExprObject::mkRefGlobal(idx));
    }
  }
}

void Environment::cast(const CastExpr &castexpr) {
  mStack.back().setPC(&castexpr);
  const auto *expr = castexpr.getSubExpr();
  assert(expr);
  if (mStack.back().containsStmt(expr)) {
    auto val = getStmtVal(*expr);
    mStack.back().insertStmt(&castexpr, ExprObject::mkVal(val));
  }
}

Environment::FunctionCallVisitorAction
Environment::call(const CallExpr *pcallexpr) {
  mStack.back().setPC(pcallexpr);
  ValueTy val = 0;
  const FunctionDecl *pcallee = pcallexpr->getDirectCallee();
  const auto &call = assertDeref(pcallee);
  const auto &callexpr = assertDeref(pcallexpr);
  if (pcallee == mInput) {
    llvm::errs() << "Please Input an Integer Value : ";
    std::cin >> val;

    mStack.back().insertStmt(pcallexpr, ExprObject::mkVal(val));
    return FunctionCallVisitorAction::mkIgnore();
  } else if (pcallee == mOutput) {
    const auto *expr = callexpr.getArg(0);
    assert(expr);
    val = getStmtVal(*expr);
    llvm::errs() << val;
    return FunctionCallVisitorAction::mkIgnore();
  } else if (pcallee == mMalloc) {
    const auto *expr = callexpr.getArg(0);
    const auto val = getStmtVal(assertDeref(expr));

    // Directly call malloc, this is properly aligned.
    const auto object =
        ExprObject::mkVal(reinterpret_cast<ValueTy>(malloc(val)));

    mStack.back().insertStmt(pcallexpr, object);
    return FunctionCallVisitorAction::mkIgnore();
  } else if (pcallee == mFree) {
    return FunctionCallVisitorAction::mkIgnore();
  } else {
    const auto *pdef = pcallee->getDefinition();
    assert(pdef && "Undefined function called!");
    auto &def = *pdef;

    // Prepare a new stack for this function call.
    StackFrame newFrame;
    unsigned numArgs = callexpr.getNumArgs();
    for (unsigned i = 0; i < numArgs; i++) {
      const auto *pparam = def.getParamDecl(i);
      assert(pparam);
      auto argVal = getStmtVal(assertDeref(callexpr.getArg(i)));
      newFrame.allocDecl(pparam, argVal);
    }

    mStack.push_back(newFrame);

    // Notify the visitor that it should jump to the function body
    // and evaluate it.
    return FunctionCallVisitorAction::mkVisitBody({&def});
  }
}

const clang::FunctionDecl &
Environment::FunctionCallVisitorAction::getFunctionToVisit() const {
  assert(mKind == Kind::VISIT_BODY);
  return assertDeref(mVBPayload.mDecl);
}

void Environment::returnStmt(const ReturnStmt &ret) {
  const Expr *value = ret.getRetValue();
  mStack.back().setReturn(getStmtVal(assertDeref(value)));
}

void Environment::callExit() {
  assert(!mStack.empty());

  StackFrame calleeFrame = mStack.back();
  mStack.pop_back();

  assert(!mStack.empty());

  StackFrame &callerFrame = mStack.back();

  // Set the value of "callexpr" in caller frame to "return"-ed value.
  ValueTy returnedValue = calleeFrame.getReturn();

  callerFrame.insertStmt(callerFrame.getPC(), ExprObject::mkVal(returnedValue));
}

void Environment::registerGlobalVar(const VarDecl &var, ValueTy value) {
  mGlobalFrame.allocDecl(&var, value);
}

void Environment::registerGlobalVarFromStack(const VarDecl &var, Stmt &init) {
  auto value = getStmtVal(init);
  registerGlobalVar(var, value);
}

ValueTy Environment::getDeclVal(const Decl &decl) const {
  const auto &topFrame = mStack.back();
  if (topFrame.containsDecl(&decl)) {
    return topFrame.getDeclVal(&decl);
  }
  return mGlobalFrame.getDeclVal(&decl);
}

ValueTy Environment::getStmtVal(const Stmt &s) const {
  auto v = mStack.back().getStmt(&s);

  if (v.isLValue()) {
    return getExpr(v);
  }

  return v.getData().mVal;
}

ValueTy &Environment::refStack(std::size_t idx) {
  return mStack.back().refValueAt(idx);
}

ValueTy &Environment::refGlobal(std::size_t idx) {
  return mGlobalFrame.refValueAt(idx);
}

ValueTy &Environment::refExpr(ExprObject v) {
  switch (v.getKind()) {
  case ExprObject::ValueKind::REF_STACK:
    return refStack(v.getData().mStackIndex);
  case ExprObject::ValueKind::REF_HEAP:
    return *reinterpret_cast<ValueTy *>(v.getData().mHeapIndex);
  case ExprObject::ValueKind::VAL:
    break;
  case ExprObject::ValueKind::REF_GLOBAL:
    return refGlobal(v.getData().mGlobalIndex);
  }

  assert(false && "unexpected rvalue assignment");
  __builtin_unreachable();
}

ValueTy Environment::getStack(std::size_t idx) const {
  return mStack.back().getValueAt(idx);
}

ValueTy Environment::getGlobal(std::size_t idx) const {
  return mGlobalFrame.getValueAt(idx);
}

ValueTy Environment::getExpr(ExprObject v) const {
  switch (v.getKind()) {
  case ExprObject::ValueKind::REF_STACK:
    return getStack(v.getData().mStackIndex);
  case ExprObject::ValueKind::REF_HEAP:
    return *reinterpret_cast<ValueTy *>(v.getData().mHeapIndex);
  case ExprObject::ValueKind::VAL:
    break;
  case ExprObject::ValueKind::REF_GLOBAL:
    return getGlobal(v.getData().mGlobalIndex);
  }

  assert(false && "unexpected rvalue assignment");
  __builtin_unreachable();
}
