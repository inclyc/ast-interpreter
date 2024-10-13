//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool
//--------------===//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

#include "Environment.h"
#include "Support.h"

#include <exception>

using namespace clang;

namespace {

struct ReturnException : std::exception {};

} // namespace

class InterpreterVisitor : public EvaluatedExprVisitor<InterpreterVisitor> {
public:
  explicit InterpreterVisitor(const ASTContext &context, Environment *env)
      : EvaluatedExprVisitor(context), mEnv(env) {}

  void VisitIntegerLiteral(IntegerLiteral *expr) {
    mEnv->integerLiteral(*expr);
  }

  void VisitBinaryOperator(BinaryOperator *bop) {
    VisitStmt(bop);
    mEnv->binop(bop);
  }
  void VisitDeclRefExpr(DeclRefExpr *expr) {
    VisitStmt(expr);
    mEnv->declref(expr);
  }
  void VisitCastExpr(CastExpr *expr) {
    VisitStmt(expr);
    mEnv->cast(expr);
  }
  void VisitCallExpr(CallExpr *call) {
    VisitStmt(call);
    using VisitorAction = Environment::FunctionCallVisitorAction;
    VisitorAction visitorAction = mEnv->call(call);
    switch (visitorAction.kind()) {
    case VisitorAction::Kind::IGNORE:
      break;
    case VisitorAction::Kind::VISIT_BODY: {
      FunctionDecl &func = visitorAction.getFunctionToVisit();
      try {
        Visit(func.getBody());
      } catch (ReturnException &e) {
      }
      mEnv->callExit();
      break;
    }
    }
  }

  void VisitReturnStmt(ReturnStmt *preturnStmt) {
    VisitStmt(preturnStmt);
    mEnv->returnStmt(assertDeref(preturnStmt));
    throw ReturnException();
  }

  void VisitDeclStmt(DeclStmt *declstmt) { mEnv->decl(declstmt); }

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
    try {
      mVisitor.VisitStmt(entry->getBody());
    } catch (ReturnException &) {
      // Entry function "return"s, just ignore it.
    }
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
