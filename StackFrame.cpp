#include "StackFrame.h"

StackFrame::StackFrame() : mVars(), mExprs(), mPC(), mReturn() {}

void StackFrame::setReturn(ValueTy val) { mReturn = val; }

ValueTy StackFrame::getReturn() const { return mReturn; }

void StackFrame::bindDecl(clang::Decl *decl, ValueTy val) { mVars[decl] = val; }

ValueTy StackFrame::getDeclVal(clang::Decl *decl) {
  if (mVars.find(decl) != mVars.end()) {
    return mVars.find(decl)->second;
  }
  throw NoSuchDeclException();
}

void StackFrame::bindStmt(clang::Stmt *stmt, ValueTy val) {
  mExprs[stmt] = val;
}

ValueTy StackFrame::getStmtVal(clang::Stmt *stmt) {
  assert(mExprs.find(stmt) != mExprs.end());
  return mExprs[stmt];
}

void StackFrame::setPC(clang::Stmt *stmt) { mPC = stmt; }

clang::Stmt *StackFrame::getPC() { return mPC; }
