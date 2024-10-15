//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool
//--------------===//
//===----------------------------------------------------------------------===//

#include "StackFrame.h"
#include "Support.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"

using GlobalVarMap = std::map<clang::Decl *, VariableValueTy>;

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
      clang::FunctionDecl *mDecl = nullptr;
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

    [[nodiscard]] clang::FunctionDecl &getFunctionToVisit();

    [[nodiscard]] Kind kind() const { return mKind; }
  };

private:
  std::vector<StackFrame> mStack;

  GlobalVarMap mGlovalVars;

  clang::FunctionDecl *mFree; /// Declartions to the built-in functions
  clang::FunctionDecl *mMalloc;
  clang::FunctionDecl *mInput;
  clang::FunctionDecl *mOutput;

  clang::FunctionDecl *mEntry;

  void bindStmt(clang::Stmt &s, VariableValueTy val);

public:
  /// Get the declartions to the built-in functions
  Environment();

  /// Record integer literals
  void integerLiteral(clang::IntegerLiteral &literal);

  void registerGlobalVar(clang::VarDecl &var, VariableValueTy value);
  void registerGlobalVarFromStack(clang::VarDecl &var, clang::Stmt &init);

  /// Initialize the Environment
  void init(clang::TranslationUnitDecl *unit);

  clang::FunctionDecl *getEntry();

  void binop(clang::BinaryOperator *bop);

  void unaryOp(clang::UnaryOperator &unaryOp);

  void decl(clang::DeclStmt *declstmt);
  void declref(clang::DeclRefExpr *declref);

  void cast(clang::CastExpr *castexpr);

  [[nodiscard]] FunctionCallVisitorAction call(clang::CallExpr *callexpr);

  void returnStmt(clang::ReturnStmt &ret);

  /// The function call exited. Switch the context to caller.
  void callExit();

  VariableValueTy getDeclVal(clang::Decl &decl);
  VariableValueTy getStmtVal(clang::Stmt &s);
};
