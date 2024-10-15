#pragma once

#include "Support.h"

#include <clang/AST/Expr.h>

#include <exception>

struct NoSuchDeclException : std::exception {
  const char *what() const noexcept override { return "No such declaration"; }
};

class StackFrame {
  /// StackFrame maps Variable Declaration to Value
  /// Which are either integer or addresses (also represented using an Integer
  /// value)
  std::map<clang::Decl *, ValueTy> mVars;
  std::map<clang::Stmt *, ValueTy> mExprs;
  /// The current stmt
  clang::Stmt *mPC;

  ValueTy mReturn;

public:
  StackFrame();

  void setReturn(ValueTy val);
  ValueTy getReturn() const;

  void bindDecl(clang::Decl *decl, ValueTy val);
  ValueTy getDeclVal(clang::Decl *decl);
  void bindStmt(clang::Stmt *stmt, ValueTy val);
  ValueTy getStmtVal(clang::Stmt *stmt);
  void setPC(clang::Stmt *stmt);
  clang::Stmt *getPC();
};
