#include "StackFrame.h"
#include "Support.h"

StackFrame::StackFrame() : mVars(), mExprs(), mPC(), mReturn() {}

void StackFrame::setReturn(ValueTy val) { mReturn = val; }

ValueTy StackFrame::getReturn() const { return mReturn; }

ValueTy StackFrame::getValueAt(std::size_t idx) const { return mData.at(idx); };

ValueTy &StackFrame::refValueAt(std::size_t idx) { return mData.at(idx); };

std::size_t StackFrame::allocDecl(const clang::Decl *decl, ValueTy init,
                                  std::size_t n) {
  auto size = mData.size();
  mData.resize(size + n, init);
  mVars.insert({decl, size});
  return size;
};

bool StackFrame::containsDecl(const clang::Decl *decl) const {
  return mVars.count(decl);
}

std::size_t StackFrame::getDeclIdx(const clang::Decl *decl) const {
  return mVars.at(decl);
}

ValueTy StackFrame::getDeclVal(const clang::Decl *decl) const {
  auto idx = getDeclIdx(decl);
  return getValueAt(idx);
};

ValueTy &StackFrame::refDeclVal(const clang::Decl *decl) {
  auto idx = getDeclIdx(decl);
  return refValueAt(idx);
};

bool StackFrame::containsStmt(const clang::Stmt *stmt) const {
  return mExprs.count(stmt);
};

ExprObject StackFrame::getStmt(const clang::Stmt *stmt) const {
  return mExprs.at(stmt);
};

ExprObject &StackFrame::refStmt(const clang::Stmt *stmt) {
  return mExprs.at(stmt);
};

void StackFrame::insertStmt(const clang::Stmt *stmt, ExprObject val) {
  if (mExprs.count(stmt)) {
    mExprs.at(stmt) = val;
  } else {
    mExprs.insert({stmt, val});
  }
};

void StackFrame::setPC(const clang::Stmt *stmt) { mPC = stmt; }

const clang::Stmt *StackFrame::getPC() { return mPC; }
