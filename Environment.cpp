#include "Environment.h"
#include "Support.h"

#include <cassert>
#include <iostream>

Environment::Environment()
    : mStack(), mFree(nullptr), mMalloc(nullptr), mInput(nullptr),
      mOutput(nullptr), mEntry(nullptr) {}

void Environment::integerLiteral(IntegerLiteral &literal) {
  llvm::APInt value = literal.getValue();
  assert(value.getBitWidth() <= 32);
  bindStmt(literal, value.getSExtValue());
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
    int val = getStmtVal(*right);
    bindStmt(*left, val);
    if (DeclRefExpr *declexpr = dyn_cast<DeclRefExpr>(left)) {
      Decl *decl = declexpr->getFoundDecl();
      mStack.back().bindDecl(decl, val);
    }
  } else {
    VariableValueTy lhs = getStmtVal(*left);
    VariableValueTy rhs = getStmtVal(*right);

    // BinaryOP : + | - | * | / | < | > | ==
    switch (bop->getOpcode()) {
    case clang::BO_Add:
      bindStmt(*bop, lhs + rhs);
      break;
    case clang::BO_Sub:
      bindStmt(*bop, lhs - rhs);
      break;
    case clang::BO_Mul:
      bindStmt(*bop, lhs * rhs);
      break;
    case clang::BO_Div:
      bindStmt(*bop, lhs / rhs);
      break;
    case clang::BO_LT:
      bindStmt(*bop, lhs < rhs);
      break;
    case clang::BO_GT:
      bindStmt(*bop, lhs > rhs);
      break;
    case clang::BO_EQ:
      bindStmt(*bop, lhs == rhs);
      break;
    default:
      assert(false && "unexpected binary operation!");
      break;
    }
  }
}

void Environment::decl(DeclStmt *declstmt) {
  for (DeclStmt::decl_iterator it = declstmt->decl_begin(),
                               ie = declstmt->decl_end();
       it != ie; ++it) {
    Decl *decl = *it;
    if (VarDecl *vardecl = dyn_cast<VarDecl>(decl)) {
      Expr *init = vardecl->getInit();
      VariableValueTy initValue = init ? getStmtVal(*init) : 0;
      mStack.back().bindDecl(vardecl, initValue);
    }
  }
}
void Environment::declref(DeclRefExpr *declref) {
  mStack.back().setPC(declref);
  if (declref->getType()->isIntegerType()) {
    Decl *decl = declref->getFoundDecl();
    assert(decl);
    bindStmt(*declref, getDeclVal(*decl));
  }
}

void Environment::cast(CastExpr *castexpr) {
  mStack.back().setPC(castexpr);
  assert(castexpr);
  if (castexpr->getType()->isIntegerType()) {
    Expr *expr = castexpr->getSubExpr();
    VariableValueTy val = mStack.back().getStmtVal(expr);
    bindStmt(*castexpr, val);
  }
}

Environment::FunctionCallVisitorAction Environment::call(CallExpr *pcallexpr) {
  mStack.back().setPC(pcallexpr);
  VariableValueTy val = 0;
  FunctionDecl *pcallee = pcallexpr->getDirectCallee();
  auto &call = assertDeref(pcallee);
  auto &callexpr = assertDeref(pcallexpr);
  if (pcallee == mInput) {
    llvm::errs() << "Please Input an Integer Value : ";
    std::cin >> val;

    bindStmt(callexpr, val);
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
      newFrame.bindDecl(pparam, getStmtVal(assertDeref(callexpr.getArg(i))));
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
  VariableValueTy returnedValue = calleeFrame.getReturn();
  callerFrame.bindStmt(callerFrame.getPC(), returnedValue);
}

void Environment::registerGlobalVar(VarDecl &var, VariableValueTy value) {
  assert(mGlovalVars.count(&var) == 0);
  mGlovalVars.insert({&var, value});
}

void Environment::registerGlobalVarFromStack(VarDecl &var, Stmt &init) {
  auto value = getStmtVal(init);
  registerGlobalVar(var, value);
}

int Environment::getDeclVal(Decl &decl) {
  try {
    return mStack.back().getDeclVal(&decl);
  } catch (NoSuchDeclException &) {
    // If the variable is not defined in function stack, it is in global vars.
    return mGlovalVars.at(&decl);
  }
}

VariableValueTy Environment::getStmtVal(Stmt &s) {
  return mStack.back().getStmtVal(&s);
}

void Environment::bindStmt(Stmt &s, VariableValueTy val) {
  mStack.back().bindStmt(&s, val);
}
