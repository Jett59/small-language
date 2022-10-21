#include "ast.h"
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

std::unique_ptr<Type> decayReferenceType(std::unique_ptr<Type> type) {
  if (type->type == TypeType::REFERENCE) {
    return static_cast<const ReferenceTypeNode &>(*type).type->clone();
  } else {
    return type;
  }
}

void CompilationUnitNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  for (auto &definition : definitions) {
    definition->assignType(symbolTable);
  }
}
void DefinitionNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  initializer->assignType(symbolTable);
  if (!initializer->valueType) {
    throw std::runtime_error("Initializer has no type");
  }
  valueType = decayReferenceType(initializer->valueType->get()->clone());
  symbolTable.emplace(name, *this);
}
void IntegerLiteralNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  valueType =
      std::make_unique<PrimitiveTypeNode>(getSmallestTypeThatContains(value));
}
void FloatLiteralNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::F64);
}
void StringLiteralNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::STRING);
}
void BoolLiteralNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::BOOL);
}
void FunctionNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  auto newSymbolTable = symbolTable;
  for (const auto &parameter : parameters) {
    // Create a fake definition node for the parameter.
    auto definitionNode = DefinitionNode{parameter->name, nullptr, "let"};
    definitionNode.valueType = parameter->type->clone();
    newSymbolTable.emplace(parameter->name, std::move(definitionNode));
  }
  for (auto &statement : body) {
    statement->assignType(newSymbolTable);
  }
  std::vector<std::unique_ptr<Type>> parameterTypes;
  for (auto &parameter : parameters) {
    parameterTypes.push_back(parameter->type->clone());
  }
  valueType = std::make_unique<FunctionTypeNode>(std::move(parameterTypes),
                                                 returnType->clone());
}

static inline bool isComparisonOperator(BinaryOperatorType operatorType) {
  return operatorType == BinaryOperatorType::EQUAL ||
         operatorType == BinaryOperatorType::NOT_EQUAL ||
         operatorType == BinaryOperatorType::LESS_THAN ||
         operatorType == BinaryOperatorType::LESS_THAN_OR_EQUAL ||
         operatorType == BinaryOperatorType::GREATER_THAN ||
         operatorType == BinaryOperatorType::GREATER_THAN_OR_EQUAL;
}

void BinaryOperatorNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  left->assignType(symbolTable);
  right->assignType(symbolTable);
  if (!left->valueType || !right->valueType) {
    throw std::runtime_error("Binary operator has no type");
  }
  if (operatorType == BinaryOperatorType::ASSIGN) {
    const auto &leftType = *left->valueType;
    const auto &rightType = *right->valueType;
    if (leftType->type != TypeType::REFERENCE) {
      throw std::runtime_error("Left side of assignment is not a reference");
    }
    const auto &leftReferenceType =
        static_cast<const ReferenceTypeNode &>(*leftType);
    if (leftReferenceType.constant) {
      throw std::runtime_error("Left side of assignment is a constant");
    }
    if (leftReferenceType.type->equals(*rightType)) {
      valueType = leftReferenceType.clone();
    } else {
      throw std::runtime_error(
          "Left and right side of assignment have different types: " +
          leftType->toString() + " and " + rightType->toString());
    }
  } else {
    auto leftType = decayReferenceType(left->valueType->get()->clone());
    auto rightType = decayReferenceType(right->valueType->get()->clone());
    if (!leftType->equals(*rightType)) {
      throw std::runtime_error(
          "Left and right side of assignment have different types: " +
          leftType->toString() + " and " + rightType->toString());
    }
    if (isComparisonOperator(operatorType)) {
      valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::BOOL);
    } else {
      valueType = std::move(leftType);
    }
  }
}
void NilNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::NIL);
}
void IfStatementNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  auto newSymbolTable = symbolTable;
  condition->assignType(newSymbolTable);
  if (!condition->valueType) {
    throw std::runtime_error("If condition has no type");
  }
  if (condition->valueType->get()->type != TypeType::PRIMITIVE ||
      static_cast<const PrimitiveTypeNode &>(*condition->valueType->get())
              .primitiveType != PrimitiveType::BOOL) {
    throw std::runtime_error("If condition is not a boolean");
  }
  for (auto &statement : thenBody) {
    statement->assignType(newSymbolTable);
  }
  for (auto &statement : elseBody) {
    statement->assignType(symbolTable);
  }
}
void VariableReferenceNode::assignType(
    std::map<std::string, const DefinitionNode &> &symbolTable) {
  auto typeEntry = symbolTable.find(name);
  if (typeEntry == symbolTable.end()) {
    throw std::runtime_error("Variable " + name + " not found");
  }
  valueType = std::make_unique<ReferenceTypeNode>(
      typeEntry->second.valueType->get()->clone(), typeEntry->second.constant);
}
} // namespace sl
