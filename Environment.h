//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool
//--------------===//
//===----------------------------------------------------------------------===//

#include "StackFrame.h"
#include "Support.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"

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
      const clang::FunctionDecl *mDecl = nullptr;
    };

  private:
    const Kind mKind;
    const VisitBodyPayload mVBPayload;

    FunctionCallVisitorAction(Kind kind, VisitBodyPayload vbpayload)
        : mKind(kind), mVBPayload(vbpayload) {}

  public:
    static FunctionCallVisitorAction mkIgnore() {
      return FunctionCallVisitorAction(Kind::IGNORE, {});
    }

    static FunctionCallVisitorAction mkVisitBody(VisitBodyPayload payload) {
      return {Kind::VISIT_BODY, {payload}};
    }

    [[nodiscard]] const clang::FunctionDecl &getFunctionToVisit() const;

    [[nodiscard]] Kind kind() const { return mKind; }
  };

private:
  std::vector<StackFrame> mStack;

  StackFrame mGlobalFrame;

  const clang::FunctionDecl *mFree; /// Declartions to the built-in functions
  const clang::FunctionDecl *mMalloc;
  const clang::FunctionDecl *mInput;
  const clang::FunctionDecl *mOutput;

  const clang::FunctionDecl *mEntry;

public:
  /// Get the declartions to the built-in functions
  Environment();

  void arraySubscript(const clang::ArraySubscriptExpr &asub);

  /// Record integer literals
  void integerLiteral(const clang::IntegerLiteral &literal);

  void unaryExprOrTypeTrait(const clang::UnaryExprOrTypeTraitExpr &ute);

  void registerGlobalVar(const clang::VarDecl &var, ValueTy value);
  void registerGlobalVarFromStack(const clang::VarDecl &var, clang::Stmt &init);

  /// Initialize the Environment
  void init(const clang::TranslationUnitDecl &unit);

  const clang::FunctionDecl *getEntry() const;

  void binop(const clang::BinaryOperator &bop);

  void unaryOp(const clang::UnaryOperator &unaryOp);

  void decl(const clang::DeclStmt &declstmt);
  void declref(const clang::DeclRefExpr &declref);

  void cast(const clang::CastExpr &castexpr);

  [[nodiscard]] FunctionCallVisitorAction call(const clang::CallExpr *callexpr);

  void returnStmt(const clang::ReturnStmt &ret);

  /// The function call exited. Switch the context to caller.
  void callExit();

  ValueTy getDeclVal(const clang::Decl &decl) const;
  ValueTy getStmtVal(const clang::Stmt &s) const;

  ValueTy &refStack(std::size_t idx);
  ValueTy &refGlobal(std::size_t idx);
  ValueTy &refExpr(ExprObject v);
  ValueTy getStack(std::size_t idx) const;
  ValueTy getGlobal(std::size_t idx) const;
  ValueTy getExpr(ExprObject v) const;
};
