#include "StackFrame.h"

StackFrame::StackFrame() : mVars(), mExprs(), mPC(), mReturn() {}

void StackFrame::setReturn(VariableValueTy val) { mReturn = val; }

VariableValueTy StackFrame::getReturn() const { return mReturn; }

void StackFrame::bindDecl(clang::Decl *decl, VariableValueTy val) {
  mVars[decl] = val;
}

VariableValueTy StackFrame::getDeclVal(clang::Decl *decl) {
  if (mVars.find(decl) != mVars.end()) {
    return mVars.find(decl)->second;
  }
  throw NoSuchDeclException();
}

void StackFrame::bindStmt(clang::Stmt *stmt, VariableValueTy val) {
  mExprs[stmt] = val;
}

VariableValueTy StackFrame::getStmtVal(clang::Stmt *stmt) {
  assert(mExprs.find(stmt) != mExprs.end());
  return mExprs[stmt];
}

void StackFrame::setPC(clang::Stmt *stmt) { mPC = stmt; }

clang::Stmt *StackFrame::getPC() { return mPC; }
