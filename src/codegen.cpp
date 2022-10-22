#include "codegen.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include <iostream>

namespace sl {
using namespace llvm;

struct FunctionContext {
  Function *function;
  BasicBlock *block;
  IRBuilder<> irBuilder;
};

static llvm::Type *getLlvmType(const sl::Type &type, LLVMContext &context) {
  switch (type.type) {
  case TypeType::PRIMITIVE: {
    const PrimitiveTypeNode &primitiveType =
        static_cast<const PrimitiveTypeNode &>(type);
    switch (primitiveType.primitiveType) {
    case PrimitiveType::U8:
    case PrimitiveType::I8:
      return llvm::Type::getInt8Ty(context);
    case PrimitiveType::U16:
    case PrimitiveType::I16:
      return llvm::Type::getInt16Ty(context);
    case PrimitiveType::U32:
    case PrimitiveType::I32:
      return llvm::Type::getInt32Ty(context);
    case PrimitiveType::U64:
    case PrimitiveType::I64:
      return llvm::Type::getInt64Ty(context);
    case PrimitiveType::F32:
      return llvm::Type::getFloatTy(context);
    case PrimitiveType::F64:
      return llvm::Type::getDoubleTy(context);
    case PrimitiveType::BOOL:
      return llvm::Type::getInt1Ty(context);
    case PrimitiveType::CHAR:
      return llvm::Type::getInt32Ty(context);
    case PrimitiveType::NIL:
      return llvm::Type::getVoidTy(context);
    default:
      throw std::runtime_error("Unknown primitive type");
    }
  }
  case TypeType::FUNCTION: {
    const FunctionTypeNode &functionType =
        static_cast<const FunctionTypeNode &>(type);
    std::vector<llvm::Type *> parameterTypes;
    for (const auto &parameterType : functionType.arguments) {
      parameterTypes.push_back(getLlvmType(*parameterType, context));
    }
    llvm::Type *returnType = getLlvmType(*functionType.returnType, context);
    return llvm::FunctionType::get(returnType, parameterTypes, false);
  }
  case TypeType::REFERENCE: {
    const ReferenceTypeNode &referenceType =
        static_cast<const ReferenceTypeNode &>(type);
    return getLlvmType(*referenceType.type, context)->getPointerTo();
  }
  default:
    throw std::runtime_error("Unknown type");
  }
}

static void codegenStatement(const AstNode &statement, LLVMContext &context,
                             Module &module, FunctionContext &function);

static Value *codegenExpression(const AstNode &expression, LLVMContext &context,
                                Module &module,
                                FunctionContext &currentFunction) {
  switch (expression.type) {
  case AstNodeType::FUNCTION: {
    const FunctionNode &functionNode =
        static_cast<const FunctionNode &>(expression);
    Function *function =
        Function::Create(static_cast<FunctionType *>(
                             getLlvmType(**expression.valueType, context)),
                         GlobalValue::ExternalLinkage, "#func", &module);
    BasicBlock *entryBlock = BasicBlock::Create(context, "entry", function);
    FunctionContext newFunctionContext{function, entryBlock,
                                       IRBuilder<>(entryBlock)};
    for (const auto &statement : functionNode.body) {
      codegenStatement(*statement, context, module, newFunctionContext);
    }
    if (PrimitiveTypeNode{PrimitiveType::NIL}.equals(
            *static_cast<const FunctionTypeNode &>(**functionNode.valueType)
                 .returnType)) {
      newFunctionContext.irBuilder.CreateRetVoid();
    }
    return function;
  }
  case AstNodeType::INTEGER_LITERAL: {
    const IntegerLiteralNode &integerLiteralNode =
        static_cast<const IntegerLiteralNode &>(expression);
    return ConstantInt::get(getLlvmType(**expression.valueType, context),
                            integerLiteralNode.value);
  }
  case AstNodeType::FLOAT_LITERAL: {
    const FloatLiteralNode &floatLiteralNode =
        static_cast<const FloatLiteralNode &>(expression);
    return ConstantFP::get(getLlvmType(**expression.valueType, context),
                           floatLiteralNode.value);
  }
  case AstNodeType::BOOL_LITERAL: {
    const BoolLiteralNode &boolLiteralNode =
        static_cast<const BoolLiteralNode &>(expression);
    return ConstantInt::get(getLlvmType(**expression.valueType, context),
                            boolLiteralNode.value);
  }
  default:
    throw std::runtime_error("Unknown expression type");
  }
}

static void codegenGlobalDefinition(const DefinitionNode &definition,
                                    LLVMContext &context, Module &module,
                                    FunctionContext &initFunction) {
  Value *initializerValue =
      codegenExpression(*definition.initializer, context, module, initFunction);
  // Determine if the initializer is a constant value.
  bool isConstantExpression = isa<Constant>(initializerValue);
  GlobalVariable *globalVariable = new GlobalVariable(
      initializerValue->getType(), isConstantExpression && definition.constant,
      GlobalValue::InternalLinkage, nullptr, definition.name);
  module.getGlobalList().push_back(globalVariable);
  if (isConstantExpression) {
    globalVariable->setInitializer(cast<Constant>(initializerValue));
  } else {
    initFunction.irBuilder.CreateStore(initializerValue, globalVariable);
  }
}

static void codegenStatement(const AstNode &statement, LLVMContext &context,
                             Module &module, FunctionContext &function) {
  switch (statement.type) {
  case AstNodeType::IF_STATEMENT: {
    throw std::runtime_error("if statements not implemented");
  }
  default:
    codegenExpression(statement, context, module, function);
  }
}

void codegen(const AstNode &ast, const std::string &initialTargetTriple) {
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  LLVMContext llvmContext;
  Module module("sl", llvmContext);
  if (ast.type != AstNodeType::COMPILATION_UNIT) {
    throw std::runtime_error("Expected compilation unit");
  }
  Function *rawDefinitionsFunction = Function::Create(
      FunctionType::get(llvm::Type::getVoidTy(llvmContext), false),
      GlobalValue::ExternalLinkage, "#init", &module);
  BasicBlock *rawDefinitionsEntryBlock =
      BasicBlock::Create(llvmContext, "entry", rawDefinitionsFunction);
  FunctionContext rawDefinitionsFunctionContext{
      rawDefinitionsFunction, rawDefinitionsEntryBlock,
      IRBuilder<>(rawDefinitionsEntryBlock)};
  const CompilationUnitNode &compilationUnit =
      static_cast<const CompilationUnitNode &>(ast);
  for (const auto &statement : compilationUnit.definitions) {
    if (statement->type != AstNodeType::DEFINITION) {
      codegenStatement(*statement, llvmContext, module,
                       rawDefinitionsFunctionContext);
    } else {
      codegenGlobalDefinition(static_cast<const DefinitionNode &>(*statement),
                              llvmContext, module,
                              rawDefinitionsFunctionContext);
    }
  }
  rawDefinitionsFunctionContext.irBuilder.CreateRetVoid();
  std::string targetTriple;
  if (initialTargetTriple == "") {
    targetTriple = sys::getDefaultTargetTriple();
  } else {
    targetTriple = initialTargetTriple;
  }
  std::string err;
  auto target = TargetRegistry::lookupTarget(targetTriple, err);
  if (!target) {
    throw std::runtime_error(err);
  }
  auto cpu = "generic";
  auto features = "";
  TargetOptions opt;
  auto rm = Optional<Reloc::Model>();
  auto targetMachine =
      target->createTargetMachine(targetTriple, cpu, features, opt, rm);
  module.setTargetTriple(targetTriple);
  module.setDataLayout(targetMachine->createDataLayout());
  module.print(errs(), nullptr);
}
} // namespace sl
