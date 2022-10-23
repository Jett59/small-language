#include "ast.h"
#include "error.h"
#include <iostream>
#include <string>

namespace sl {
using std::string_literals::operator""s;

std::string CompilationUnitNode::toString() const {
  std::string result = "ComiplationUnit {\n";
  for (const auto &definition : definitions) {
    result += definition->toString() + "\n";
  }
  result += "}";
  return result;
}
std::string DefinitionNode::toString() const {
  return (constant ? "Constant"s : "Mutable"s) + " definition " + name + " = " +
         initializer->toString();
}
std::string IntegerLiteralNode::toString() const {
  return "Integer " + std::to_string(value);
}
std::string FloatLiteralNode::toString() const {
  return "Float " + std::to_string(value);
}
std::string StringLiteralNode::toString() const {
  return "String \""s + value + "\"";
}
std::string BoolLiteralNode::toString() const {
  return "Bool "s + (value ? "true"s : "false"s);
}
std::string FunctionNode::toString() const {
  std::string result = "Function:"s;
  for (const auto &param : parameters) {
    result += " " + param->name + ": " + param->type->toString();
  }
  result += " -> "s + returnType->toString() + " {\n"s;
  for (const auto &statement : body) {
    result += "\t"s + statement->toString() + "\n"s;
  }
  result += "}";
  return result;
}
std::string BinaryOperatorNode::toString() const {
  return std::string("BinaryOperatorExpression ("s +
                     binaryOperatorTypeToString[operatorType] + ") " +
                     left->toString() + ", " + right->toString());
}
std::string NilNode::toString() const { return "nil"s; }
std::string IfStatementNode::toString() const {
  std::string result = "if "s + condition->toString() + " {\n"s;
  for (const auto &statement : thenBody) {
    result += "\t"s + statement->toString() + "\n"s;
  }
  if (elseBody.size() > 0) {
    result += "} else {\n"s;
    for (const auto &statement : elseBody) {
      result += "\t"s + statement->toString() + "\n"s;
    }
  }
  result += "}";
  return result;
}
std::string VariableReferenceNode::toString() const {
  return "VariableReference: "s + name;
}
std::string CastNode::toString() const {
  return "Cast: "s + (*valueType)->toString() + " " + value->toString();
}
std::string CallNode::toString() const {
  std::string result = "Call: "s + function->toString() + "("s;
  for (const auto &arg : arguments) {
    result += arg->toString() + ", "s;
  }
  result += ")";
  return result;
}

std::unique_ptr<Type> decayReferenceType(std::unique_ptr<Type> type) {
  if (type->type == TypeType::REFERENCE) {
    const auto &referenceType = static_cast<const ReferenceTypeNode &>(*type);
    if (referenceType.type->type != TypeType::FUNCTION) {
      return referenceType.type->clone();
    } else {
      return type;
    }
  } else {
    return type;
  }
}

void CompilationUnitNode::assignType(SymbolTable &symbolTable,
                                     const SymbolTable &) {
  SymbolTable allGlobalDefinitions;
  for (auto &definition : definitions) {
    if (definition->type == AstNodeType::DEFINITION) {
      auto &definitionNode = static_cast<DefinitionNode &>(*definition);
      // Run the type assignment with global symbols as empty, thus skipping all
      // function body type assignment.
      definitionNode.assignType(allGlobalDefinitions, {});
    }
  }
  for (auto &definition : definitions) {
    definition->assignType(symbolTable, allGlobalDefinitions);
  }
}
void DefinitionNode::assignType(SymbolTable &symbolTable,
                                const SymbolTable &allGlobalSymbols) {
  initializer->assignType(symbolTable, allGlobalSymbols);
  if (!initializer->valueType) {
    throw SlException(initializer->line, initializer->column,
                      "Initializer has no type");
  }
  valueType = decayReferenceType(initializer->valueType->get()->clone());
  symbolTable.emplace(name, *this);
}
void IntegerLiteralNode::assignType(SymbolTable &symbolTable,
                                    const SymbolTable &) {
  valueType =
      std::make_unique<PrimitiveTypeNode>(getSmallestTypeThatContains(value));
}
void FloatLiteralNode::assignType(SymbolTable &symbolTable,
                                  const SymbolTable &) {
  valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::F64);
}
void StringLiteralNode::assignType(SymbolTable &symbolTable,
                                   const SymbolTable &) {
  valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::STRING);
}
void BoolLiteralNode::assignType(SymbolTable &symbolTable,
                                 const SymbolTable &) {
  valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::BOOL);
}
void FunctionNode::assignType(SymbolTable &symbolTable,
                              const SymbolTable &allGlobalSymbols) {
  if (!allGlobalSymbols.empty()) {
    auto newSymbolTable = allGlobalSymbols;
    // To avoid fake definitions from going out of scope too soon.
    std::vector<std::unique_ptr<DefinitionNode>> fakeDefinitions;
    fakeDefinitions.reserve(parameters.size());
    for (const auto &parameter : parameters) {
      // Create a fake definition node for the parameter.
      std::unique_ptr<DefinitionNode> definition =
          std::make_unique<DefinitionNode>(parameter->name, nullptr, "let",
                                           parameter->type->clone());
      fakeDefinitions.push_back(std::move(definition));
      newSymbolTable.emplace(parameter->name, *fakeDefinitions.back());
    }
    for (auto &statement : body) {
      statement->assignType(newSymbolTable, allGlobalSymbols);
    }
  }
  std::vector<std::unique_ptr<Type>> parameterTypes;
  for (auto &parameter : parameters) {
    parameterTypes.push_back(parameter->type->clone());
  }
  valueType = std::make_unique<ReferenceTypeNode>(
      std::make_unique<FunctionTypeNode>(std::move(parameterTypes),
                                         returnType->clone()),
      true);
}

static inline bool isComparisonOperator(BinaryOperatorType operatorType) {
  return operatorType == BinaryOperatorType::EQUAL ||
         operatorType == BinaryOperatorType::NOT_EQUAL ||
         operatorType == BinaryOperatorType::LESS_THAN ||
         operatorType == BinaryOperatorType::LESS_THAN_OR_EQUAL ||
         operatorType == BinaryOperatorType::GREATER_THAN ||
         operatorType == BinaryOperatorType::GREATER_THAN_OR_EQUAL;
}

void BinaryOperatorNode::assignType(SymbolTable &symbolTable,
                                    const SymbolTable &allGlobalSymbols) {
  left->assignType(symbolTable, allGlobalSymbols);
  right->assignType(symbolTable, allGlobalSymbols);
  if (!left->valueType || !right->valueType) {
    throw SlException(line, column, "Binary operator has no type");
  }
  if (operatorType == BinaryOperatorType::ASSIGN) {
    const auto &leftType = *left->valueType;
    auto rightType = decayReferenceType((*right->valueType)->clone());
    if (leftType->type != TypeType::REFERENCE) {
      throw SlException(line, column,
                        "Left side of assignment is not a reference");
    }
    const auto &leftReferenceType =
        static_cast<const ReferenceTypeNode &>(*leftType);
    if (leftReferenceType.constant) {
      throw SlException(line, column, "Left side of assignment is a constant");
    }
    if (isIntegral(*leftReferenceType.type) &&
        right->type == AstNodeType::INTEGER_LITERAL) {
      rightType = decayReferenceType(leftReferenceType.type->clone());
      right->valueType = rightType->clone();
    } else if (isFloat(*leftReferenceType.type) &&
               right->type == AstNodeType::FLOAT_LITERAL) {
      rightType = decayReferenceType(leftReferenceType.type->clone());
      right->valueType = rightType->clone();
    }
    if (leftReferenceType.type->equals(*rightType)) {
      valueType = leftReferenceType.clone();
    } else {
      throw SlException(
          line, column,
          "Left and right side of assignment have different types: " +
              leftType->toString() + " and " + rightType->toString());
    }
  } else {
    auto leftType = decayReferenceType(left->valueType->get()->clone());
    auto rightType = decayReferenceType(right->valueType->get()->clone());
    if (isIntegral(*leftType) && right->type == AstNodeType::INTEGER_LITERAL) {
      rightType = leftType->clone();
      right->valueType = leftType->clone();
    } else if (isFloat(*leftType) &&
               right->type == AstNodeType::FLOAT_LITERAL) {
      rightType = leftType->clone();
      right->valueType = leftType->clone();
    }
    if (!leftType->equals(*rightType)) {
      throw SlException(
          line, column,
          "Left and right side have different types: " + leftType->toString() +
              " and " + rightType->toString());
    }
    if (isComparisonOperator(operatorType)) {
      valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::BOOL);
    } else {
      valueType = std::move(leftType);
    }
  }
}
void NilNode::assignType(SymbolTable &, const SymbolTable &) {
  valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::NIL);
}
void IfStatementNode::assignType(SymbolTable &symbolTable,
                                 const SymbolTable &allGlobalSymbols) {
  auto newSymbolTable = symbolTable;
  condition->assignType(newSymbolTable, allGlobalSymbols);
  if (!condition->valueType) {
    throw SlException(line, column, "If condition has no type");
  }
  auto conditionType = decayReferenceType(condition->valueType->get()->clone());
  if (conditionType->type != TypeType::PRIMITIVE ||
      static_cast<const PrimitiveTypeNode &>(*conditionType).primitiveType !=
          PrimitiveType::BOOL) {
    throw SlException(line, column, "If condition is not a boolean");
  }
  for (auto &statement : thenBody) {
    statement->assignType(newSymbolTable, allGlobalSymbols);
  }
  for (auto &statement : elseBody) {
    statement->assignType(symbolTable, allGlobalSymbols);
  }
}
void VariableReferenceNode::assignType(SymbolTable &symbolTable,
                                       const SymbolTable &) {
  auto typeEntry = symbolTable.find(name);
  if (typeEntry == symbolTable.end()) {
    throw SlException(line, column, "Variable " + name + " not found");
  }
  valueType = std::make_unique<ReferenceTypeNode>(
      typeEntry->second.valueType->get()->clone(), typeEntry->second.constant);
}
void CastNode::assignType(SymbolTable &symbolTable,
                          const SymbolTable &allGlobalSymbols) {
  value->assignType(symbolTable, allGlobalSymbols);
  if (!value->valueType) {
    throw SlException(line, column, "Cast expression has no type");
  }
}
void CallNode::assignType(SymbolTable &symbolTable,
                          const SymbolTable &allGlobalSymbols) {
  function->assignType(symbolTable, allGlobalSymbols);
  auto functionExpressionType =
      decayReferenceType((*function->valueType)->clone());
  if (functionExpressionType->type != TypeType::REFERENCE) {
    throw SlException(line, column,
                      "Function call is not a reference to a function");
  }
  auto &functionType =
      static_cast<const ReferenceTypeNode &>(*functionExpressionType).type;
  if (functionType->type != TypeType::FUNCTION) {
    throw SlException(line, column, "Function call is not a function");
  }
  auto &functionTypeNode = static_cast<const FunctionTypeNode &>(*functionType);
  if (functionTypeNode.arguments.size() != arguments.size()) {
    throw SlException(line, column, "Not enough arguments to function call");
  }
  for (size_t i = 0; i < arguments.size(); ++i) {
    arguments[i]->assignType(symbolTable, allGlobalSymbols);
    if (!arguments[i]->valueType) {
      throw SlException(line, column,
                        "Argument " + std::to_string(i) + " has no type");
    }
    auto argumentType =
        decayReferenceType(arguments[i]->valueType->get()->clone());
    if (!argumentType->equals(*functionTypeNode.arguments[i])) {
      throw SlException(line, column,
                        "Argument " + std::to_string(i + 1) +
                            " has wrong type: " + argumentType->toString() +
                            ", expecting " +
                            functionTypeNode.arguments[i]->toString());
    }
  }
  valueType = functionTypeNode.returnType->clone();
}
} // namespace sl
