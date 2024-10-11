#include "Environment.h"

Environment::Environment()
    : mStack(), mFree(nullptr), mMalloc(nullptr), mInput(nullptr),
      mOutput(nullptr), mEntry(nullptr) {}

void Environment::integerLiteral(IntegerLiteral &Literal) {
  llvm::APInt Value = Literal.getValue();
  assert(Value.getBitWidth() <= 32);
  mStack.back().bindStmt(&Literal, Value.getSExtValue());
}

void Environment::init(TranslationUnitDecl *unit) {
  for (TranslationUnitDecl::decl_iterator i = unit->decls_begin(),
                                          e = unit->decls_end();
       i != e; ++i) {
    if (FunctionDecl *fdecl = dyn_cast<FunctionDecl>(*i)) {
      if (fdecl->getName().equals("FREE"))
        mFree = fdecl;
      else if (fdecl->getName().equals("MALLOC"))
        mMalloc = fdecl;
      else if (fdecl->getName().equals("GET"))
        mInput = fdecl;
      else if (fdecl->getName().equals("PRINT"))
        mOutput = fdecl;
      else if (fdecl->getName().equals("main"))
        mEntry = fdecl;
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

    int val = mStack.back().getDeclVal(decl);
    mStack.back().bindStmt(declref, val);
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
