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
  std::map<clang::Decl *, VariableValueTy> mVars;
  std::map<clang::Stmt *, VariableValueTy> mExprs;
  /// The current stmt
  clang::Stmt *mPC;

  VariableValueTy mReturn;

public:
  StackFrame();

  void setReturn(VariableValueTy val);
  VariableValueTy getReturn() const;

  void bindDecl(clang::Decl *decl, VariableValueTy val);
  VariableValueTy getDeclVal(clang::Decl *decl);
  void bindStmt(clang::Stmt *stmt, VariableValueTy val);
  VariableValueTy getStmtVal(clang::Stmt *stmt);
  void setPC(clang::Stmt *stmt);
  clang::Stmt *getPC();
};
