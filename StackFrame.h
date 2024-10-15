#pragma once

#include "ExprValue.h"
#include "Support.h"

#include <clang/AST/Expr.h>

#include <exception>

struct NoSuchDeclException : std::exception {
  const char *what() const noexcept override { return "No such declaration"; }
};

class StackFrame {
  std::vector<ValueTy> mData;

  /// StackFrame maps Variable Declaration to Value
  /// Which are either integer or addresses (also represented using an Integer
  /// value)
  std::map<clang::Decl *, std::size_t> mVars;
  std::map<clang::Stmt *, ExprObject> mExprs;

  /// The current stmt
  clang::Stmt *mPC;

  ValueTy mReturn;

public:
  StackFrame();

  void setReturn(ValueTy val);
  ValueTy getReturn() const;

  ValueTy getValueAt(std::size_t idx) const;
  ValueTy &refValueAt(std::size_t idx);

  std::size_t allocDecl(clang::Decl *decl, ValueTy init = 0, std::size_t n = 1);
  bool containsDecl(clang::Decl *decl);
  std::size_t getDeclIdx(clang::Decl *decl) const;
  ValueTy getDeclVal(clang::Decl *decl) const;
  ValueTy &refDeclVal(clang::Decl *decl);

  bool containsStmt(clang::Stmt *stmt);
  ExprObject getStmt(clang::Stmt *stmt) const;
  ExprObject &refStmt(clang::Stmt *stmt);
  void insertStmt(clang::Stmt *stmt, ExprObject val);

  void setPC(clang::Stmt *stmt);
  clang::Stmt *getPC();
};
