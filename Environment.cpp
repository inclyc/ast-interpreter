#include "Environment.h"

Environment::Environment()
    : mStack(), mFree(nullptr), mMalloc(nullptr), mInput(nullptr),
      mOutput(nullptr), mEntry(nullptr) {}

void Environment::integerLiteral(IntegerLiteral &literal) {
  llvm::APInt value = literal.getValue();
  assert(value.getBitWidth() <= 32);
  mStack.back().bindStmt(&literal, value.getSExtValue());
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
  Expr *left = bop->getLHS();
  Expr *right = bop->getRHS();

  if (bop->isAssignmentOp()) {
    int val = mStack.back().getStmtVal(right);
    mStack.back().bindStmt(left, val);
    if (DeclRefExpr *declexpr = dyn_cast<DeclRefExpr>(left)) {
      Decl *decl = declexpr->getFoundDecl();
      mStack.back().bindDecl(decl, val);
    }
  }
}

void Environment::decl(DeclStmt *declstmt) {
  for (DeclStmt::decl_iterator it = declstmt->decl_begin(),
                               ie = declstmt->decl_end();
       it != ie; ++it) {
    Decl *decl = *it;
    if (VarDecl *vardecl = dyn_cast<VarDecl>(decl)) {
      mStack.back().bindDecl(vardecl, 0);
    }
  }
}
void Environment::declref(DeclRefExpr *declref) {
  mStack.back().setPC(declref);
  if (declref->getType()->isIntegerType()) {
    Decl *decl = declref->getFoundDecl();
    assert(decl);
    mStack.back().bindStmt(declref, lookupDeclValue(*decl));
  }
}

void Environment::cast(CastExpr *castexpr) {
  mStack.back().setPC(castexpr);
  if (castexpr->getType()->isIntegerType()) {
    Expr *expr = castexpr->getSubExpr();
    int val = mStack.back().getStmtVal(expr);
    mStack.back().bindStmt(castexpr, val);
  }
}

void Environment::call(CallExpr *callexpr) {
  mStack.back().setPC(callexpr);
  int val = 0;
  FunctionDecl *callee = callexpr->getDirectCallee();
  if (callee == mInput) {
    llvm::errs() << "Please Input an Integer Value : ";
    scanf("%d", &val);

    mStack.back().bindStmt(callexpr, val);
  } else if (callee == mOutput) {
    Expr *decl = callexpr->getArg(0);
    val = mStack.back().getStmtVal(decl);
    llvm::errs() << val;
  } else {
    /// You could add your code here for Function call Return
  }
}

void Environment::registerGlobalVar(VarDecl &var, VariableValueTy value) {
  assert(mGlovalVars.count(&var) == 0);
  mGlovalVars.insert({&var, value});
}

void Environment::registerGlobalVarFromStack(VarDecl &var, Stmt &init) {
  auto value = mStack.back().getStmtVal(&init);
  registerGlobalVar(var, value);
}

int Environment::lookupDeclValue(Decl &decl) {
  try {
    return mStack.back().getDeclVal(&decl);
  } catch (NoSuchDeclException &) {
    // If the variable is not defined in function stack, it is in global vars.
    return mGlovalVars.at(&decl);
  }
}
