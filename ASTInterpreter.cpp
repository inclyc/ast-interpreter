//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool
//--------------===//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

using namespace clang;

#include "Environment.h"

class InterpreterVisitor : public EvaluatedExprVisitor<InterpreterVisitor> {
public:
  explicit InterpreterVisitor(const ASTContext &context, Environment *env)
      : EvaluatedExprVisitor(context), mEnv(env) {}
  virtual ~InterpreterVisitor() {}

  void VisitIntegerLiteral(IntegerLiteral *expr) {
    mEnv->integerLiteral(*expr);
  }

  virtual void VisitBinaryOperator(BinaryOperator *bop) {
    VisitStmt(bop);
    mEnv->binop(bop);
  }
  virtual void VisitDeclRefExpr(DeclRefExpr *expr) {
    VisitStmt(expr);
    mEnv->declref(expr);
  }
  virtual void VisitCastExpr(CastExpr *expr) {
    VisitStmt(expr);
    mEnv->cast(expr);
  }
  virtual void VisitCallExpr(CallExpr *call) {
    VisitStmt(call);
    mEnv->call(call);
  }
  virtual void VisitDeclStmt(DeclStmt *declstmt) { mEnv->decl(declstmt); }

private:
  Environment *mEnv;
};

class InterpreterConsumer : public ASTConsumer {
public:
  explicit InterpreterConsumer(const ASTContext &context)
      : mEnv(), mVisitor(context, &mEnv) {}
  virtual ~InterpreterConsumer() {}

  virtual void HandleTranslationUnit(clang::ASTContext &context) {
    TranslationUnitDecl *decl = context.getTranslationUnitDecl();
    mEnv.init(decl);

    // Process global variables.
    {
      auto _ = Environment::GlobalVarRAII(mEnv);
      for (auto topDecl : decl->decls()) {
        if (auto varDecl = llvm::dyn_cast<VarDecl>(topDecl)) {
          VarDecl &var = *varDecl;
          Expr *init = var.getInit();

          if (init) {
            mVisitor.Visit(init);
            mEnv.registerGlobalVarFromStack(var, *init);
          } else {
            mEnv.registerGlobalVar(var, 0);
          }
        }
      }
    }

    FunctionDecl *entry = mEnv.getEntry();
    mVisitor.VisitStmt(entry->getBody());
  }

private:
  Environment mEnv;
  InterpreterVisitor mVisitor;
};

class InterpreterClassAction : public ASTFrontendAction {
public:
  virtual std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &compiler, llvm::StringRef inFile) {
    return std::unique_ptr<clang::ASTConsumer>(
        new InterpreterConsumer(compiler.getASTContext()));
  }
};

int main(int argc, char **argv) {
  if (argc > 1) {
    clang::tooling::runToolOnCode(
        std::unique_ptr<clang::FrontendAction>(new InterpreterClassAction),
        argv[1]);
  }
}
