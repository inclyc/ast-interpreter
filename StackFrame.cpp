#include "StackFrame.h"
#include "Support.h"

StackFrame::StackFrame() : mVars(), mExprs(), mPC(), mReturn() {}

void StackFrame::setReturn(ValueTy val) { mReturn = val; }

ValueTy StackFrame::getReturn() const { return mReturn; }

ValueTy StackFrame::getValueAt(std::size_t idx) const { return mData.at(idx); };

ValueTy &StackFrame::refValueAt(std::size_t idx) { return mData.at(idx); };

std::size_t StackFrame::allocDecl(clang::Decl *decl, ValueTy init,
                                  std::size_t n) {
  auto size = mData.size();
  mData.resize(size + n, init);
  mVars.insert({decl, size});
  return size;
};

bool StackFrame::containsDecl(clang::Decl *decl) { return mVars.count(decl); }

std::size_t StackFrame::getDeclIdx(clang::Decl *decl) const {
  return mVars.at(decl);
}

ValueTy StackFrame::getDeclVal(clang::Decl *decl) const {
  auto idx = getDeclIdx(decl);
  return getValueAt(idx);
};

ValueTy &StackFrame::refDeclVal(clang::Decl *decl) {
  auto idx = getDeclIdx(decl);
  return refValueAt(idx);
};

bool StackFrame::containsStmt(clang::Stmt *stmt) { return mExprs.count(stmt); };

ExprObject StackFrame::getStmt(clang::Stmt *stmt) const {
  return mExprs.at(stmt);
};

ExprObject &StackFrame::refStmt(clang::Stmt *stmt) { return mExprs.at(stmt); };

void StackFrame::insertStmt(clang::Stmt *stmt, ExprObject val) {
  if (mExprs.count(stmt)) {
    mExprs.at(stmt) = val;
  } else {
    mExprs.insert({stmt, val});
  }
};

void StackFrame::setPC(clang::Stmt *stmt) { mPC = stmt; }

clang::Stmt *StackFrame::getPC() { return mPC; }
