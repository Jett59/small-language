#include "codegen.h"
#include "stringHelper.h"
#include <iostream>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>

namespace sl {
using namespace llvm;

struct FunctionContext {
  Function *function;
  BasicBlock *block;
  IRBuilder<> irBuilder;
  bool isMainFunction = false;
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
  case TypeType::ARRAY: {
    const ArrayType &arrayType = static_cast<const ArrayType &>(type);
    // Arrays are  structs with a pointer and a size.
    return llvm::StructType::get(
        context, {getLlvmType(*arrayType.type, context)->getPointerTo(),
                  llvm::Type::getInt64Ty(context)});
  }
  default:
    throw std::runtime_error("Unknown type");
  }
}

static const Type &removeReference(const Type &type) {
  return type.type == TypeType::REFERENCE
             ? *static_cast<const ReferenceTypeNode &>(type).type
             : type;
}

using SymbolTable = std::map<std::string, Value *>;

static void createAssert(FunctionContext &function, Value *condition,
                         const std::string &message, Module &module,
                         LLVMContext &context) {
  BasicBlock *assertBlock =
      BasicBlock::Create(context, "assert", function.function);
  BasicBlock *continueBlock =
      BasicBlock::Create(context, "assert.continue", function.function);
  function.irBuilder.CreateCondBr(condition, continueBlock, assertBlock);
  function.irBuilder.SetInsertPoint(assertBlock);
  CallInst *panicCall = function.irBuilder.CreateCall(
      module.getFunction("panic"),
      std::vector<Value *>{function.irBuilder.CreateGlobalStringPtr(message)});
  panicCall->setDoesNotReturn();
  panicCall->setTailCall();
  function.irBuilder.CreateUnreachable();
  function.irBuilder.SetInsertPoint(continueBlock);
}

static void codegenStatement(const AstNode &statement, LLVMContext &context,
                             Module &module, FunctionContext &function,
                             SymbolTable &symbolTable,
                             const SymbolTable &allGlobalSymbols);

static Value *codegenExpression(const AstNode &expression, LLVMContext &context,
                                Module &module,
                                FunctionContext &currentFunction,
                                SymbolTable &symbolTable,
                                const SymbolTable &allGlobalSymbols) {
  switch (expression.type) {
  case AstNodeType::SUBSCRIPT: {
    const SubscriptNode &subscript =
        static_cast<const SubscriptNode &>(expression);
    Value *array =
        codegenExpression(*subscript.value, context, module, currentFunction,
                          symbolTable, allGlobalSymbols);
    Value *index =
        codegenExpression(*subscript.index, context, module, currentFunction,
                          symbolTable, allGlobalSymbols);
    Value *size = currentFunction.irBuilder.CreateExtractValue(array, 1);
    createAssert(currentFunction,
                 currentFunction.irBuilder.CreateICmpULT(index, size),
                 "Array index out of bounds", module, context);
    Value *pointer = currentFunction.irBuilder.CreateExtractValue(array, 0);
    return currentFunction.irBuilder.CreateInBoundsGEP(
        getLlvmType(removeReference(**subscript.valueType), context), pointer,
        std::vector<Value *>{index});
  }
  case AstNodeType::DEREFERENCE: {
    const DereferenceNode &dereference =
        static_cast<const DereferenceNode &>(expression);
    Value *value =
        codegenExpression(*dereference.value, context, module, currentFunction,
                          symbolTable, allGlobalSymbols);
    return currentFunction.irBuilder.CreateLoad(
        getLlvmType(**dereference.valueType, context), value);
  }
  case AstNodeType::FUNCTION: {
    const FunctionNode &functionNode =
        static_cast<const FunctionNode &>(expression);
    const FunctionTypeNode &functionType =
        static_cast<const FunctionTypeNode &>(
            removeReference(**functionNode.valueType));
    Function *function = Function::Create(
        static_cast<FunctionType *>(getLlvmType(functionType, context)),
        GlobalValue::InternalLinkage, "#func", &module);
    BasicBlock *entryBlock = BasicBlock::Create(context, "entry", function);
    FunctionContext newFunctionContext{function, entryBlock,
                                       IRBuilder<>(entryBlock)};
    SymbolTable newSymbolTable = allGlobalSymbols;
    for (size_t i = 0; i < functionNode.parameters.size(); i++) {
      const auto &parameter = functionNode.parameters[i];
      // For consistency with other ways of defining variables, we put this into
      // an alloca. This ensures that everywhere which is expecting a reference
      // will work.
      AllocaInst *alloca = newFunctionContext.irBuilder.CreateAlloca(
          getLlvmType(*parameter->type, context), nullptr, parameter->name);
      newFunctionContext.irBuilder.CreateStore(function->arg_begin() + i,
                                               alloca);
      newSymbolTable[parameter->name] = alloca;
    }
    for (const auto &statement : functionNode.body) {
      codegenStatement(*statement, context, module, newFunctionContext,
                       newSymbolTable, allGlobalSymbols);
    }
    if (PrimitiveTypeNode{PrimitiveType::NIL}.equals(
            *functionType.returnType)) {
      newFunctionContext.irBuilder.CreateRetVoid();
    }
    return function;
  }
  case AstNodeType::EXTERNAL: {
    const auto &externalNode = static_cast<const ExternalNode &>(expression);
    const auto &externalType = removeReference(**externalNode.valueType);
    if (externalType.type == TypeType::FUNCTION) {
      FunctionType *functionType =
          static_cast<FunctionType *>(getLlvmType(externalType, context));
      return module.getOrInsertFunction(externalNode.name, functionType)
          .getCallee();
    } else {
      return module.getOrInsertGlobal(externalNode.name,
                                      getLlvmType(externalType, context));
    }
  }
  case AstNodeType::CALL: {
    const CallNode &callNode = static_cast<const CallNode &>(expression);
    Value *function =
        codegenExpression(*callNode.function, context, module, currentFunction,
                          symbolTable, allGlobalSymbols);
    std::vector<Value *> argumentValues;
    argumentValues.reserve(callNode.arguments.size());
    for (const auto &argument : callNode.arguments) {
      Value *argumentValue =
          codegenExpression(*argument, context, module, currentFunction,
                            symbolTable, allGlobalSymbols);
      argumentValues.push_back(argumentValue);
    }
    // Get the function's type. The function value is a pointer so that won't
    // do.
    if (!callNode.function->valueType ||
        removeReference(**callNode.function->valueType).type !=
            TypeType::FUNCTION) {
      throw std::runtime_error("Can only call functions");
    }
    const FunctionTypeNode &functionType =
        static_cast<const FunctionTypeNode &>(
            removeReference(**callNode.function->valueType));
    Value *result = currentFunction.irBuilder.CreateCall(
        static_cast<FunctionType *>(getLlvmType(functionType, context)),
        function, argumentValues);
    return result;
  }
  case AstNodeType::CAST: {
    const CastNode &castNode = static_cast<const CastNode &>(expression);
    Value *value =
        codegenExpression(*castNode.value, context, module, currentFunction,
                          symbolTable, allGlobalSymbols);
    if (castNode.valueType->get()->equals(**castNode.value->valueType)) {
      return value;
    }
    bool isResultValueIntegral = isIntegral(**castNode.valueType);
    bool isResultValueFloat = isFloat(**castNode.valueType);
    bool isOriginalValueIntegral = value->getType()->isIntegerTy();
    bool isOriginalValueFloat = value->getType()->isFloatingPointTy();
    if (isResultValueIntegral && isOriginalValueIntegral) {
      return currentFunction.irBuilder.CreateIntCast(
          value, getLlvmType(**expression.valueType, context),
          isSigned(**castNode.valueType));
    } else if (isResultValueFloat && isOriginalValueFloat) {
      return currentFunction.irBuilder.CreateFPCast(
          value, getLlvmType(**expression.valueType, context));
    } else if (isResultValueIntegral && isOriginalValueFloat) {
      if (isSigned(**castNode.valueType)) {
        return currentFunction.irBuilder.CreateFPToSI(
            value, getLlvmType(**expression.valueType, context));
      } else {
        return currentFunction.irBuilder.CreateFPToUI(
            value, getLlvmType(**expression.valueType, context));
      }
    } else if (isResultValueFloat && isOriginalValueIntegral) {
      if (isSigned(**castNode.value->valueType) ||
          isRefToSigned(**castNode.value->valueType)) {
        return currentFunction.irBuilder.CreateSIToFP(
            value, getLlvmType(**expression.valueType, context));
      } else {
        return currentFunction.irBuilder.CreateUIToFP(
            value, getLlvmType(**expression.valueType, context));
      }
    } else {
      throw std::runtime_error("Unknown cast");
    }
  }
  case AstNodeType::VARIABLE_REFERENCE: {
    const VariableReferenceNode &variableReferenceNode =
        static_cast<const VariableReferenceNode &>(expression);
    return symbolTable[variableReferenceNode.name];
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
    const auto &boolLiteralNode =
        static_cast<const BooleanLiteralNode &>(expression);
    return ConstantInt::get(getLlvmType(**expression.valueType, context),
                            boolLiteralNode.value);
  }
  case AstNodeType::STRING_LITERAL: {
    const auto &stringLiteralNode =
        static_cast<const StringLiteralNode &>(expression);
    std::string value = translateEscapes(stringLiteralNode.value);
    Constant *stringPointer =
        currentFunction.irBuilder.CreateGlobalStringPtr(value);
    return ConstantStruct::get(
        static_cast<StructType *>(getLlvmType(**expression.valueType, context)),
        {stringPointer,
         ConstantInt::get(llvm::Type::getInt64Ty(context), value.size())});
  }
  case AstNodeType::ARRAY_LITERAL: {
    const auto &arrayLiteralNode =
        static_cast<const ArrayLiteralNode &>(expression);
    const auto &arrayType = static_cast<const ArrayType &>(
        removeReference(**arrayLiteralNode.valueType));
    const auto &elementType = removeReference(*arrayType.type);
    auto llvmElementType = getLlvmType(elementType, context);
    auto llvmArrayType = getLlvmType(arrayType, context);
    std::vector<Value *> values;
    for (size_t i = 0; i < arrayLiteralNode.values.size(); i++) {
      Value *value =
          codegenExpression(*arrayLiteralNode.values[i], context, module,
                            currentFunction, symbolTable, allGlobalSymbols);
      values.push_back(value);
    }
    std::vector<Constant *> constants;
    for (const auto &value : values) {
      if (isa<Constant>(value)) {
        constants.push_back(static_cast<Constant *>(value));
      } else {
        constants.push_back(Constant::getNullValue(value->getType()));
      }
    }
    Value *result = ConstantArray::get(
        static_cast<llvm::ArrayType *>(llvmArrayType), constants);
    for (size_t i = 0; i < values.size(); i++) {
      if (!isa<Constant>(values[i])) {
        result = currentFunction.irBuilder.CreateInsertValue(
            result, values[i], {static_cast<unsigned>(i)});
      }
    }
    Value *arrayPointer;
    llvm::Type *arrayPointerType = llvmElementType->getPointerTo();
    if (currentFunction.isMainFunction) { // Main runs once so we can use a
                                          // global variable.
      Constant *initializer = isa<Constant>(result)
                                  ? static_cast<Constant *>(result)
                                  : Constant::getNullValue(result->getType());
      GlobalVariable *globalVariable = new GlobalVariable(
          initializer->getType(), false, GlobalValue::InternalLinkage,
          initializer, "#array");
      module.getGlobalList().push_back(globalVariable);
      arrayPointer = ConstantExpr::getBitCast(globalVariable, arrayPointerType);
    } else { // We have to resort to alloca.
      Value *alloca = currentFunction.irBuilder.CreateAlloca(arrayPointerType,
                                                             nullptr, "array");
      currentFunction.irBuilder.CreateStore(result, alloca);
      arrayPointer = currentFunction.irBuilder.CreateBitCast(
          alloca, arrayPointerType, "#array.pointer");
    }
    // Convert it to a struct with pointer and size.
    auto structType = StructType::get(
        context, std::vector<llvm::Type *>{arrayPointer->getType(),
                                           llvm::Type::getInt64Ty(context)});
    if (isa<Constant>(arrayPointer)) {
      return ConstantStruct::get(
          structType, std::vector<Constant *>{
                          static_cast<Constant *>(arrayPointer),
                          ConstantInt::get(llvm::Type::getInt64Ty(context),
                                           arrayLiteralNode.values.size())});
    } else {
      Value *structure = ConstantStruct::get(
          structType, std::vector<Constant *>{
                          Constant::getNullValue(arrayPointer->getType()),
                          ConstantInt::get(llvm::Type::getInt64PtrTy(context),
                                           arrayLiteralNode.values.size())});
      structure = currentFunction.irBuilder.CreateInsertValue(
          structure, arrayPointer, {0});
      return structure;
    }
  }
  case AstNodeType::BINARY_OPERATOR: {
    const BinaryOperatorNode &binaryOperatorNode =
        static_cast<const BinaryOperatorNode &>(expression);
    Value *left =
        codegenExpression(*binaryOperatorNode.left, context, module,
                          currentFunction, symbolTable, allGlobalSymbols);
    Value *right =
        codegenExpression(*binaryOperatorNode.right, context, module,
                          currentFunction, symbolTable, allGlobalSymbols);
    if (binaryOperatorNode.operatorType == BinaryOperatorType::ASSIGN) {
      return currentFunction.irBuilder.CreateStore(right, left);
    } else if (binaryOperatorNode.valueType->get()->type ==
               TypeType::PRIMITIVE) {
      PrimitiveType primitiveType = static_cast<const PrimitiveTypeNode &>(
                                        *binaryOperatorNode.operandType)
                                        .primitiveType;
      if (primitiveType == PrimitiveType::NIL) {
        throw std::runtime_error("Cannot perform binary operation on nil");
      }
      switch (binaryOperatorNode.operatorType) {
      case BinaryOperatorType::ADD:
        return currentFunction.irBuilder.CreateAdd(left, right);
      case BinaryOperatorType::SUBTRACT:
        return currentFunction.irBuilder.CreateSub(left, right);
      case BinaryOperatorType::MULTIPLY:
        return currentFunction.irBuilder.CreateMul(left, right);
      case BinaryOperatorType::DIVIDE: {
        if (isSigned(primitiveType)) {
          return currentFunction.irBuilder.CreateSDiv(left, right);
        } else {
          return currentFunction.irBuilder.CreateUDiv(left, right);
        }
      }
      case BinaryOperatorType::MODULO: {
        if (isSigned(primitiveType)) {
          return currentFunction.irBuilder.CreateSRem(left, right);
        } else {
          return currentFunction.irBuilder.CreateURem(left, right);
        }
      }
      case BinaryOperatorType::EQUAL: {
        if (isIntegral(primitiveType) || primitiveType == PrimitiveType::BOOL) {
          return currentFunction.irBuilder.CreateICmpEQ(left, right);
        } else if (isFloat(primitiveType)) {
          return currentFunction.irBuilder.CreateFCmpOEQ(left, right);
        } else {
          throw std::runtime_error("Unsupported type for equality comparison");
        }
      }
      case BinaryOperatorType::NOT_EQUAL: {
        if (isIntegral(primitiveType) || primitiveType == PrimitiveType::BOOL) {
          return currentFunction.irBuilder.CreateICmpNE(left, right);
        } else if (isFloat(primitiveType)) {
          return currentFunction.irBuilder.CreateFCmpONE(left, right);
        } else {
          throw std::runtime_error(
              "Unsupported type for inequality comparison");
        }
      }
      // We use a macro here because it is just too
      case BinaryOperatorType::LESS_THAN: {
        if (isIntegral(primitiveType)) {
          if (isSigned(primitiveType)) {
            return currentFunction.irBuilder.CreateICmpSLT(left, right);
          } else {
            return currentFunction.irBuilder.CreateICmpULT(left, right);
          }
        } else if (isFloat(primitiveType)) {
          return currentFunction.irBuilder.CreateFCmpOLT(left, right);
        } else {
          throw std::runtime_error("Unsupported type for comparison");
        }
      }
      case BinaryOperatorType::LESS_THAN_OR_EQUAL: {
        if (isIntegral(primitiveType)) {
          if (isSigned(primitiveType)) {
            return currentFunction.irBuilder.CreateICmpSLE(left, right);
          } else {
            return currentFunction.irBuilder.CreateICmpULE(left, right);
          }
        } else if (isFloat(primitiveType)) {
          return currentFunction.irBuilder.CreateFCmpOLE(left, right);
        } else {
          throw std::runtime_error("Unsupported type for comparison");
        }
      }
      case BinaryOperatorType::GREATER_THAN: {
        if (isIntegral(primitiveType)) {
          if (isSigned(primitiveType)) {
            return currentFunction.irBuilder.CreateICmpSGT(left, right);
          } else {
            return currentFunction.irBuilder.CreateICmpUGT(left, right);
          }
        } else if (isFloat(primitiveType)) {
          return currentFunction.irBuilder.CreateFCmpOGT(left, right);
        } else {
          throw std::runtime_error("Unsupported type for comparison");
        }
      }
      case BinaryOperatorType::GREATER_THAN_OR_EQUAL: {
        if (isIntegral(primitiveType)) {
          if (isSigned(primitiveType)) {
            return currentFunction.irBuilder.CreateICmpSGE(left, right);
          } else {
            return currentFunction.irBuilder.CreateICmpUGE(left, right);
          }
        } else if (isFloat(primitiveType)) {
          return currentFunction.irBuilder.CreateFCmpOGE(left, right);
        } else {
          throw std::runtime_error("Unsupported type for comparison");
        }
      }
      default:
        throw std::runtime_error("Unknown binary operator");
      }
    } else {
      throw std::runtime_error("Unsupported type for binary operator");
    }
    break;
  }
  default:
    throw std::runtime_error("Unknown expression type");
  }
}

static void codegenGlobalDefinition(const DefinitionNode &definition,
                                    LLVMContext &context, Module &module,
                                    FunctionContext &initFunction,
                                    SymbolTable &symbolTable,
                                    const SymbolTable &allGlobalSymbols) {
  Value *initializerValue =
      codegenExpression(*definition.initializer, context, module, initFunction,
                        symbolTable, allGlobalSymbols);
  bool isConstantExpression = isa<Constant>(initializerValue);
  GlobalVariable *globalVariable =
      static_cast<GlobalVariable *>(allGlobalSymbols.at(definition.name));
  globalVariable->setConstant(isConstantExpression && definition.constant);
  if (isConstantExpression) {
    globalVariable->setInitializer(cast<Constant>(initializerValue));
  } else {
    globalVariable->setInitializer(
        Constant::getNullValue(initializerValue->getType()));
    initFunction.irBuilder.CreateStore(initializerValue, globalVariable);
  }
  symbolTable[definition.name] = globalVariable;
}

static void codegenStatement(const AstNode &statement, LLVMContext &context,
                             Module &module, FunctionContext &function,
                             SymbolTable &symbolTable,
                             const SymbolTable &allGlobalSymbols) {
  switch (statement.type) {
  case AstNodeType::DEFINITION: {
    const DefinitionNode &definition =
        static_cast<const DefinitionNode &>(statement);
    Value *initializerValue =
        codegenExpression(*definition.initializer, context, module, function,
                          symbolTable, allGlobalSymbols);
    BasicBlock &entryBlock = function.function->getEntryBlock();
    IRBuilder<> irBuilder(&entryBlock, entryBlock.begin());
    AllocaInst *alloca = irBuilder.CreateAlloca(initializerValue->getType());
    function.irBuilder.CreateStore(initializerValue, alloca);
    symbolTable[definition.name] = alloca;
    break;
  }
  case AstNodeType::IF_STATEMENT: {
    const IfStatementNode &ifStatement =
        static_cast<const IfStatementNode &>(statement);
    Value *conditionValue =
        codegenExpression(*ifStatement.condition, context, module, function,
                          symbolTable, allGlobalSymbols);
    BasicBlock *trueBranch =
        BasicBlock::Create(context, "true", function.function);
    BasicBlock *falseBranch =
        BasicBlock::Create(context, "false", function.function);
    BasicBlock *mergeBranch =
        BasicBlock::Create(context, "merge", function.function);
    function.irBuilder.CreateCondBr(conditionValue, trueBranch, falseBranch);
    function.irBuilder.SetInsertPoint(trueBranch);
    SymbolTable newSymbolTable = symbolTable;
    bool truePathReturns = false;
    for (const auto &statement : ifStatement.thenBody) {
      codegenStatement(*statement, context, module, function, newSymbolTable,
                       allGlobalSymbols);
      if (statement->returns) {
        truePathReturns = true;
        break;
      }
    }
    if (!truePathReturns) {
      function.irBuilder.CreateBr(mergeBranch);
    }
    trueBranch = function.irBuilder.GetInsertBlock();
    function.irBuilder.SetInsertPoint(falseBranch);
    newSymbolTable = symbolTable;
    bool falsePathReturns = false;
    for (const auto &statement : ifStatement.elseBody) {
      codegenStatement(*statement, context, module, function, newSymbolTable,
                       allGlobalSymbols);
      if (statement->returns) {
        falsePathReturns = true;
        break;
      }
    }
    if (!falsePathReturns) {
      function.irBuilder.CreateBr(mergeBranch);
    }
    falseBranch = function.irBuilder.GetInsertBlock();
    if (!truePathReturns || !falsePathReturns) {
      function.irBuilder.SetInsertPoint(mergeBranch);
    } else {
      mergeBranch->eraseFromParent();
    }
    break;
  }
  case AstNodeType::RETURN: {
    const ReturnNode &returnNode = static_cast<const ReturnNode &>(statement);
    Value *returnValue =
        codegenExpression(*returnNode.value, context, module, function,
                          symbolTable, allGlobalSymbols);
    function.irBuilder.CreateRet(returnValue);
    break;
  }
  default:
    codegenExpression(statement, context, module, function, symbolTable,
                      allGlobalSymbols);
  }
}

void codegen(const AstNode &ast, const std::string &initialTargetTriple,
             GeneratedFileType outputFileType, const std::string &outputFile,
             bool printIr) {
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();
  LLVMContext llvmContext;
  Module module("sl", llvmContext);
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
  if (!targetMachine) {
    throw std::runtime_error("Could not allocate target machine");
  }
  module.setTargetTriple(targetTriple);
  module.setDataLayout(targetMachine->createDataLayout());
  // Needed for assertions.
  module.getOrInsertFunction(
      "panic",
      FunctionType::get(llvm::Type::getVoidTy(llvmContext),
                        {llvm::Type::getInt8PtrTy(llvmContext)}, false));
  if (ast.type != AstNodeType::COMPILATION_UNIT) {
    throw std::runtime_error("Expected compilation unit");
  }
  Function *mainFunction = Function::Create(
      FunctionType::get(llvm::Type::getInt32Ty(llvmContext), false),
      GlobalValue::ExternalLinkage, "main", &module);
  BasicBlock *mainEntryBlock =
      BasicBlock::Create(llvmContext, "entry", mainFunction);
  FunctionContext mainFunctionContext{mainFunction, mainEntryBlock,
                                      IRBuilder<>(mainEntryBlock), true};
  const CompilationUnitNode &compilationUnit =
      static_cast<const CompilationUnitNode &>(ast);
  SymbolTable allGlobalSymbols;
  for (const auto &statement : compilationUnit.definitions) {
    if (statement->type == AstNodeType::DEFINITION) {
      const DefinitionNode &definition =
          static_cast<const DefinitionNode &>(*statement);
      GlobalVariable *globalVariable = new GlobalVariable(
          getLlvmType(**statement->valueType, llvmContext), false,
          GlobalValue::InternalLinkage, nullptr, "#global");
      module.getGlobalList().push_back(globalVariable);
      allGlobalSymbols[definition.name] = globalVariable;
    }
  }
  SymbolTable symbolTable;
  for (const auto &statement : compilationUnit.definitions) {
    if (statement->type != AstNodeType::DEFINITION) {
      codegenStatement(*statement, llvmContext, module, mainFunctionContext,
                       symbolTable, allGlobalSymbols);
    } else {
      codegenGlobalDefinition(static_cast<const DefinitionNode &>(*statement),
                              llvmContext, module, mainFunctionContext,
                              symbolTable, allGlobalSymbols);
    }
  }
  mainFunctionContext.irBuilder.CreateRet(
      ConstantInt::get(llvmContext, APInt(32, 0)));
  if (verifyModule(module, &errs())) {
    module.print(errs(), nullptr);
    throw std::runtime_error("Module verification failed");
  }
  LoopAnalysisManager loopAnalysisManager;
  FunctionAnalysisManager functionAnalysisManager;
  CGSCCAnalysisManager cGSCCAnalysisManager;
  ModuleAnalysisManager moduleAnalysisManager;
  PassBuilder passBuilder;
  passBuilder.registerModuleAnalyses(moduleAnalysisManager);
  passBuilder.registerCGSCCAnalyses(cGSCCAnalysisManager);
  passBuilder.registerFunctionAnalyses(functionAnalysisManager);
  passBuilder.registerLoopAnalyses(loopAnalysisManager);
  passBuilder.crossRegisterProxies(loopAnalysisManager, functionAnalysisManager,
                                   cGSCCAnalysisManager, moduleAnalysisManager);
  ModulePassManager modulePassManager =
      passBuilder.buildPerModuleDefaultPipeline(OptimizationLevel::O3);
  modulePassManager.run(module, moduleAnalysisManager);
  if (printIr) {
    module.print(errs(), nullptr);
  }
  std::error_code ec;
  raw_fd_ostream dest(outputFile, ec, sys::fs::OF_None);
  if (ec) {
    throw std::runtime_error("Could not open file: " + ec.message());
  }
  auto fileType = outputFileType == GeneratedFileType::OBJECT
                      ? CGFT_ObjectFile
                      : CGFT_AssemblyFile;
  legacy::PassManager legacyPassManager;
  if (targetMachine->addPassesToEmitFile(legacyPassManager, dest, nullptr,
                                         fileType)) {
    throw std::runtime_error("TargetMachine can't emit a file of this type");
  }
  legacyPassManager.run(module);
  dest.flush();
}
} // namespace sl
