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

void Environment::integerLiteral(IntegerLiteral &literal) {
  llvm::APInt value = literal.getValue();
  assert(value.getBitWidth() <= 32);
  mStack.back().insertStmt(&literal, ExprObject::mkVal(value.getZExtValue()));
}

void Environment::init(TranslationUnitDecl *unit) {
  for (TranslationUnitDecl::decl_iterator i = unit->decls_begin(),
                                          e = unit->decls_end();
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

clang::FunctionDecl *Environment::getEntry() { return mEntry; }
void Environment::binop(BinaryOperator *bop) {
  assert(bop);
  Expr *left = bop->getLHS();
  Expr *right = bop->getRHS();

  assert(left && right);

  if (bop->isAssignmentOp()) {
    auto val = getStmtVal(*right);
    auto leftObj = mStack.back().getStmt(left);
    auto &refLeft = refExpr(leftObj);
    refLeft = val;

    // Propogate the lvalue to binary op itself.
    // Thus we can support continous assignments.
    mStack.back().insertStmt(bop, leftObj);
  } else {
    ValueTy lhs = getStmtVal(*left);
    ValueTy rhs = getStmtVal(*right);

    ValueTy val;

    // BinaryOP : + | - | * | / | < | > | ==
    switch (bop->getOpcode()) {
    case clang::BO_Add:
      val = lhs + rhs;
      break;
    case clang::BO_Sub:
      val = lhs - rhs;
      break;
    case clang::BO_Mul:
      val = lhs * rhs;
      break;
    case clang::BO_Div:
      val = lhs / rhs;
      break;
    case clang::BO_LT:
      val = lhs < rhs;
      break;
    case clang::BO_GT:
      val = lhs > rhs;
      break;
    case clang::BO_EQ:
      val = lhs == rhs;
      break;
    default:
      assert(false && "unexpected binary operation!");
      break;
    }

    // The binary op evaluates to rvalue "val"
    mStack.back().insertStmt(bop, ExprObject::mkVal(val));
  }
}

void Environment::unaryOp(UnaryOperator &unaryOp) {
  ValueTy val = getStmtVal(assertDeref(unaryOp.getSubExpr()));
  assert(unaryOp.getOpcode() == clang::UO_Minus);
  mStack.back().insertStmt(&unaryOp, ExprObject::mkVal(-val));
}

void Environment::decl(DeclStmt *declstmt) {
  for (auto decl : declstmt->decls()) {
    if (VarDecl *vardecl = dyn_cast<VarDecl>(decl)) {
      Expr *init = vardecl->getInit();
      ValueTy initValue = init ? getStmtVal(*init) : 0;
      mStack.back().allocDecl(decl, initValue);
    }
  }
}
void Environment::declref(DeclRefExpr *declref) {
  mStack.back().setPC(declref);
  if (declref->getType()->isIntegerType()) {
    Decl *decl = declref->getFoundDecl();
    assert(decl);

    if (mStack.back().containsDecl(decl)) {
      // Local variable.
      auto idx = mStack.back().getDeclIdx(decl);
      // Reference stack element at "idx".
      mStack.back().insertStmt(declref, ExprObject::mkRefStack(idx));
    } else {
      // This must be a reference to global varaible.
      assert(mGlobalFrame.containsDecl(decl));
      auto idx = mGlobalFrame.getDeclIdx(decl);
      mStack.back().insertStmt(declref, ExprObject::mkRefGlobal(idx));
    }
  }
}

void Environment::cast(CastExpr *castexpr) {
  mStack.back().setPC(castexpr);
  assert(castexpr);
  if (castexpr->getType()->isIntegerType()) {
    Expr *expr = castexpr->getSubExpr();
    assert(expr);
    auto val = getStmtVal(*expr);
    mStack.back().insertStmt(castexpr, ExprObject::mkVal(val));
  }
}

Environment::FunctionCallVisitorAction Environment::call(CallExpr *pcallexpr) {
  mStack.back().setPC(pcallexpr);
  ValueTy val = 0;
  FunctionDecl *pcallee = pcallexpr->getDirectCallee();
  auto &call = assertDeref(pcallee);
  auto &callexpr = assertDeref(pcallexpr);
  if (pcallee == mInput) {
    llvm::errs() << "Please Input an Integer Value : ";
    std::cin >> val;

    mStack.back().insertStmt(pcallexpr, ExprObject::mkVal(val));
    return FunctionCallVisitorAction::mkIgnore();
  } else if (pcallee == mOutput) {
    Expr *expr = callexpr.getArg(0);
    assert(expr);
    val = getStmtVal(*expr);
    llvm::errs() << val;
    return FunctionCallVisitorAction::mkIgnore();
  } else {
    FunctionDecl *pdef = pcallee->getDefinition();
    assert(pdef && "Undefined function called!");
    auto &def = *pdef;

    // Prepare a new stack for this function call.
    StackFrame newFrame;
    unsigned numArgs = callexpr.getNumArgs();
    for (unsigned i = 0; i < numArgs; i++) {
      ParmVarDecl *pparam = def.getParamDecl(i);
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

clang::FunctionDecl &
Environment::FunctionCallVisitorAction::getFunctionToVisit() {
  assert(mKind == Kind::VISIT_BODY);
  return assertDeref(mVBPayload.mDecl);
}

void Environment::returnStmt(ReturnStmt &ret) {
  Expr *value = ret.getRetValue();
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

void Environment::registerGlobalVar(VarDecl &var, ValueTy value) {
  mGlobalFrame.allocDecl(&var, value);
}

void Environment::registerGlobalVarFromStack(VarDecl &var, Stmt &init) {
  auto value = getStmtVal(init);
  registerGlobalVar(var, value);
}

int Environment::getDeclVal(Decl &decl) {
  auto &topFrame = mStack.back();
  if (topFrame.containsDecl(&decl)) {
    return topFrame.getDeclVal(&decl);
  }
  return mGlobalFrame.getDeclVal(&decl);
}

ValueTy Environment::getStmtVal(Stmt &s) {
  auto v = mStack.back().getStmt(&s);

  if (v.isLValue()) {
    return refExpr(v);
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
  switch (v.getType()) {
  case ExprObject::ValueKind::REF_STACK:
    return refStack(v.getData().mStackIndex);
  case ExprObject::ValueKind::REF_HEAP:
    return refStack(v.getData().mHeapIndex);
  case ExprObject::ValueKind::VAL:
    break;
  case ExprObject::ValueKind::REF_GLOBAL:
    return refGlobal(v.getData().mGlobalIndex);
  }

  assert(false && "unexpected rvalue assignment");
  __builtin_unreachable();
}
