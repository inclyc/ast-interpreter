#pragma once

#include "ExprValue.h"
#include "Support.h"

#include <clang/AST/Expr.h>

class StackFrame {
  std::vector<ValueTy> mData;

  /// StackFrame maps Variable Declaration to Value
  /// Which are either integer or addresses (also represented using an Integer
  /// value)
  std::map<const clang::Decl *, std::size_t> mVars;
  std::map<const clang::Stmt *, ExprObject> mExprs;

  /// The current stmt
  const clang::Stmt *mPC;

  ValueTy mReturn;

public:
  StackFrame();

  void setReturn(ValueTy val);
  ValueTy getReturn() const;

  ValueTy getValueAt(std::size_t idx) const;
  ValueTy &refValueAt(std::size_t idx);

  std::size_t allocDecl(const clang::Decl *decl, ValueTy init = 0,
                        std::size_t n = 1);
  bool containsDecl(const clang::Decl *decl) const;
  std::size_t getDeclIdx(const clang::Decl *decl) const;
  ValueTy getDeclVal(const clang::Decl *decl) const;
  ValueTy &refDeclVal(const clang::Decl *decl);

  bool containsStmt(const clang::Stmt *stmt) const;
  ExprObject getStmt(const clang::Stmt *stmt) const;
  ExprObject &refStmt(const clang::Stmt *stmt);
  void insertStmt(const clang::Stmt *stmt, ExprObject val);

  void setPC(const clang::Stmt *stmt);
  const clang::Stmt *getPC();
};
