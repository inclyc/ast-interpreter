//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool
//--------------===//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

#include <exception>

using namespace clang;

struct NoSuchDeclException : std::exception {
  const char *what() const noexcept override { return "No such declaration"; }
};

class StackFrame {
  /// StackFrame maps Variable Declaration to Value
  /// Which are either integer or addresses (also represented using an Integer
  /// value)
  std::map<Decl *, int> mVars;
  std::map<Stmt *, int> mExprs;
  /// The current stmt
  Stmt *mPC;

public:
  StackFrame() : mVars(), mExprs(), mPC() {}

  void bindDecl(Decl *decl, int val) { mVars[decl] = val; }
  int getDeclVal(Decl *decl) {
    if (mVars.find(decl) != mVars.end()) {
      return mVars.find(decl)->second;
    }
    throw NoSuchDeclException();
  }
  void bindStmt(Stmt *stmt, int val) { mExprs[stmt] = val; }
  int getStmtVal(Stmt *stmt) {
    assert(mExprs.find(stmt) != mExprs.end());
    return mExprs[stmt];
  }
  void setPC(Stmt *stmt) { mPC = stmt; }
  Stmt *getPC() { return mPC; }
};

using VariableValueTy = int;
using GlobalVarMap = std::map<Decl *, VariableValueTy>;

/// Heap maps address to a value
/*
class Heap {
public:
   int Malloc(int size) ;
   void Free (int addr) ;
   void Update(int addr, int val) ;
   int get(int addr);
};
*/

class Environment {
public:
  /// Scope guard for constructing global vars.
  class GlobalVarRAII {
    Environment &mE;

  public:
    GlobalVarRAII(Environment &e) : mE(e) {
      mE.mStack.emplace_back(StackFrame());
    }
    ~GlobalVarRAII() { mE.mStack.pop_back(); }
  };

private:
  std::vector<StackFrame> mStack;

  GlobalVarMap mGlovalVars;

  FunctionDecl *mFree; /// Declartions to the built-in functions
  FunctionDecl *mMalloc;
  FunctionDecl *mInput;
  FunctionDecl *mOutput;

  FunctionDecl *mEntry;

  int lookupDeclValue(Decl &decl);

public:
  /// Get the declartions to the built-in functions
  Environment();

  /// Record integer literals
  void integerLiteral(IntegerLiteral &literal);

  void registerGlobalVar(VarDecl &var, VariableValueTy value);
  void registerGlobalVarFromStack(VarDecl &var, Stmt &init);

  /// Initialize the Environment
  void init(TranslationUnitDecl *unit);

  FunctionDecl *getEntry();

  /// !TODO Support comparison operation
  void binop(BinaryOperator *bop);

  void decl(DeclStmt *declstmt);
  void declref(DeclRefExpr *declref);

  void cast(CastExpr *castexpr);

  /// !TODO Support Function Call
  void call(CallExpr *callexpr);
};
