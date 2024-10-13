//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool
//--------------===//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"

#include <exception>

using namespace clang;

using VariableValueTy = int;

struct NoSuchDeclException : std::exception {
  const char *what() const noexcept override { return "No such declaration"; }
};

class StackFrame {
  /// StackFrame maps Variable Declaration to Value
  /// Which are either integer or addresses (also represented using an Integer
  /// value)
  std::map<Decl *, VariableValueTy> mVars;
  std::map<Stmt *, VariableValueTy> mExprs;
  /// The current stmt
  Stmt *mPC;

  VariableValueTy mReturn;

public:
  StackFrame() : mVars(), mExprs(), mPC(), mReturn() {}

  void setReturn(VariableValueTy val) { mReturn = val; }
  VariableValueTy getReturn() const { return mReturn; }

  void bindDecl(Decl *decl, VariableValueTy val) { mVars[decl] = val; }
  VariableValueTy getDeclVal(Decl *decl) {
    if (mVars.find(decl) != mVars.end()) {
      return mVars.find(decl)->second;
    }
    throw NoSuchDeclException();
  }
  void bindStmt(Stmt *stmt, VariableValueTy val) { mExprs[stmt] = val; }
  VariableValueTy getStmtVal(Stmt *stmt) {
    assert(mExprs.find(stmt) != mExprs.end());
    return mExprs[stmt];
  }
  void setPC(Stmt *stmt) { mPC = stmt; }
  Stmt *getPC() { return mPC; }
};

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

  class FunctionCallVisitorAction {
  public:
    enum class Kind {
      IGNORE,
      VISIT_BODY,
    };

    /// This value should be non-null if Kind == "VISIT_BODY"
    struct VisitBodyPayload {
      FunctionDecl *mDecl = nullptr;
    };

  private:
    Kind mKind;
    VisitBodyPayload mVBPayload;

    FunctionCallVisitorAction(Kind kind, VisitBodyPayload vbpayload)
        : mKind(kind), mVBPayload(vbpayload) {}

  public:
    static FunctionCallVisitorAction mkIgnore() {
      return FunctionCallVisitorAction(Kind::IGNORE, {});
    }

    static FunctionCallVisitorAction mkVisitBody(VisitBodyPayload payload) {
      return {Kind::VISIT_BODY, {payload}};
    }

    [[nodiscard]] FunctionDecl &getFunctionToVisit();

    [[nodiscard]] Kind kind() const { return mKind; }
  };

private:
  std::vector<StackFrame> mStack;

  GlobalVarMap mGlovalVars;

  FunctionDecl *mFree; /// Declartions to the built-in functions
  FunctionDecl *mMalloc;
  FunctionDecl *mInput;
  FunctionDecl *mOutput;

  FunctionDecl *mEntry;

  VariableValueTy lookupDeclValue(Decl &decl);
  void bindStmt(Stmt &s, VariableValueTy val);
  VariableValueTy getStmtVal(Stmt &s);

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

  void binop(BinaryOperator *bop);

  void decl(DeclStmt *declstmt);
  void declref(DeclRefExpr *declref);

  void cast(CastExpr *castexpr);

  [[nodiscard]] FunctionCallVisitorAction call(CallExpr *callexpr);

  void returnStmt(ReturnStmt &ret);

  /// The function call exited. Switch the context to caller.
  void callExit();
};
