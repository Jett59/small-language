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
std::string BooleanLiteralNode::toString() const {
  return "Bool "s + (value ? "true"s : "false"s);
}
std::string ArrayLiteralNode::toString() const {
  std::string result = "Array [";
  for (const auto &value : values) {
    result += value->toString() + ", ";
  }
  result += "]";
  return result;
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
std::string ReturnNode::toString() const {
  return "Return: "s + value->toString();
}
std::string ExternalNode::toString() const {
  return "ExternalFunction: "s + name + ": "s + (*valueType)->toString();
}
std::string DereferenceNode::toString() const {
  return "Dereference: "s + value->toString();
}
std::string SubscriptNode::toString() const {
  return "Subscript: "s + value->toString() + "["s + index->toString() + "]"s;
}

void decayReferenceType(std::unique_ptr<AstNode> &node) {
  const Type &type = **node->valueType;
  if (type.type == TypeType::REFERENCE) {
    const auto &referenceType = static_cast<const ReferenceTypeNode &>(type);
    // Only decay to a first-class type.
    // In LLVM, these are integers, floating points (these encompass
    // all of our primitive types except for nil, which is illegal as a
    // reference anyway), structures, arrays and references.
    // These all seem reasonable.
    if (referenceType.type->type == TypeType::PRIMITIVE ||
        referenceType.type->type == TypeType::STRUCT ||
        referenceType.type->type == TypeType::ARRAY ||
        referenceType.type->type == TypeType::REFERENCE) {
      auto dereferenceNodeType = referenceType.type->clone();
      auto dereferenceNode = std::make_unique<DereferenceNode>(std::move(node));
      dereferenceNode->valueType = std::move(dereferenceNodeType);
      node = std::move(dereferenceNode);
    }
  }
}

static void staticlyConvert(std::unique_ptr<AstNode> &value,
                            const Type &originalType, const Type &desiredType) {
                              if (originalType.equals(desiredType)) {
                                return;
                              }
  if (isIntegral(desiredType) && value->type == AstNodeType::INTEGER_LITERAL) {
    value->valueType = desiredType.clone();
  } else if (isFloat(desiredType) &&
             value->type == AstNodeType::FLOAT_LITERAL) {
    value->valueType = desiredType.clone();
  } else if (desiredType.type == TypeType::ARRAY &&
             value->type == AstNodeType::ARRAY_LITERAL) {
    auto &arrayLiteral = static_cast<ArrayLiteralNode &>(*value);
    auto &arrayType = static_cast<const ArrayType &>(desiredType);
    for (auto &element : arrayLiteral.values) {
      staticlyConvert(element, **element->valueType, *arrayType.type);
    }
    value->valueType = desiredType.clone();
  }
}

template <typename NodeClass, AstNodeType nodeType, typename VisitorType>
static void findInFunction(std::vector<std::unique_ptr<AstNode>> &body,
                           VisitorType visitor, bool enterSubFunctions) {
  for (auto &statement : body) {
    if (statement->type == nodeType) {
      visitor(static_cast<NodeClass &>(*statement));
    } else if (enterSubFunctions && statement->type == AstNodeType::FUNCTION) {
      auto &function = static_cast<FunctionNode &>(*statement);
      findInFunction<NodeClass, nodeType>(function.body, visitor,
                                          enterSubFunctions);
    } else if (statement->type == AstNodeType::IF_STATEMENT) {
      auto &ifStatement = static_cast<IfStatementNode &>(*statement);
      findInFunction<NodeClass, nodeType>(ifStatement.thenBody, visitor,
                                          enterSubFunctions);
      findInFunction<NodeClass, nodeType>(ifStatement.elseBody, visitor,
                                          enterSubFunctions);
    }
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
  findInFunction<ReturnNode, AstNodeType::RETURN>(
      definitions,
      [](ReturnNode &returnNode) {
        throw SlException(returnNode.line, returnNode.column,
                          "Returning from the root function is disallowed");
      },
      false);
}
void DefinitionNode::assignType(SymbolTable &symbolTable,
                                const SymbolTable &allGlobalSymbols) {
  initializer->assignType(symbolTable, allGlobalSymbols);
  if (!initializer->valueType) {
    throw SlException(initializer->line, initializer->column,
                      "Initializer has no type");
  }
  decayReferenceType(initializer);
  valueType = initializer->valueType->get()->clone();
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
  valueType = std::make_unique<ArrayType>(
      std::make_unique<PrimitiveTypeNode>(PrimitiveType::U8));
}
void BooleanLiteralNode::assignType(SymbolTable &symbolTable,
                                    const SymbolTable &) {
  valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::BOOL);
}
void ArrayLiteralNode::assignType(SymbolTable &symbolTable,
                                  const SymbolTable &allGlobalSymbols) {
  for (auto &value : values) {
    value->assignType(symbolTable, allGlobalSymbols);
    if (!value->valueType) {
      throw SlException(value->line, value->column, "Value has no type");
    }
    decayReferenceType(value);
  }
  if (values.size() > 0) {
    const Type &firstType = **values[0]->valueType;
    for (auto &value : values) {
      staticlyConvert(value, **value->valueType, firstType);
      if (!firstType.equals(**value->valueType)) {
        throw SlException(value->line, value->column,
                          "Array literal values must all be of the same type");
      }
    }
    valueType = std::make_unique<ArrayType>(firstType.clone());
  } else {
    valueType = std::make_unique<ArrayType>(
        std::make_unique<PrimitiveTypeNode>(PrimitiveType::NIL));
  }
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
      if (returns) {
        // Statements after the return!
        throw SlException(statement->line, statement->column,
                          "Code will never run");
      }
      statement->assignType(newSymbolTable, allGlobalSymbols);
      if (statement->returns) {
        returns = true;
      }
    }
    findInFunction<ReturnNode, AstNodeType::RETURN>(
        body,
        [this](ReturnNode &returnNode) {
          staticlyConvert(returnNode.value, **returnNode.value->valueType,
                          *returnType);
          decayReferenceType(returnNode.value);
          if (!(*returnNode.value->valueType)->equals(*returnType)) {
            throw SlException(returnNode.line, returnNode.column,
                              "Return type mismatch");
          }
        },
        false);
    if (!returns) {
      throw SlException(line, column, "Function does not necessarily return");
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
    decayReferenceType(right);
    const auto &rightType = *right->valueType;
    if (leftType->type != TypeType::REFERENCE) {
      throw SlException(line, column,
                        "Left side of assignment is not a reference");
    }
    const auto &leftReferenceType =
        static_cast<const ReferenceTypeNode &>(*leftType);
    if (leftReferenceType.constant) {
      throw SlException(line, column, "Left side of assignment is a constant");
    }
    staticlyConvert(right, *rightType, *leftReferenceType.type);
    if (leftReferenceType.type->equals(*rightType)) {
      valueType = leftReferenceType.clone();
    } else {
      throw SlException(
          line, column,
          "Left and right side of assignment have different types: " +
              leftType->toString() + " and " + rightType->toString());
    }
  } else {
    decayReferenceType(left);
    decayReferenceType(right);
    auto &leftType = *left->valueType;
    auto &rightType = *right->valueType;
    staticlyConvert(right, *rightType, *leftType);
    staticlyConvert(left, *leftType, *rightType);
    if (!leftType->equals(*rightType)) {
      throw SlException(
          line, column,
          "Left and right side have different types: " + leftType->toString() +
              " and " + rightType->toString());
    }
    if (isComparisonOperator(operatorType)) {
      valueType = std::make_unique<PrimitiveTypeNode>(PrimitiveType::BOOL);
      operandType = leftType->clone();
    } else {
      valueType = leftType->clone();
      operandType = leftType->clone();
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
  decayReferenceType(condition);
  const auto &conditionType = *condition->valueType;
  if (conditionType->type != TypeType::PRIMITIVE ||
      static_cast<const PrimitiveTypeNode &>(*conditionType).primitiveType !=
          PrimitiveType::BOOL) {
    throw SlException(line, column, "If condition is not a boolean");
  }
  bool trueBranchReturns = false;
  for (auto &statement : thenBody) {
    if (trueBranchReturns) {
      // Statements after the return!
      throw SlException(statement->line, statement->column,
                        "Code will never run");
    }
    statement->assignType(newSymbolTable, allGlobalSymbols);
    if (statement->returns) {
      trueBranchReturns = true;
    }
  }
  bool falseBranchReturns = false;
  for (auto &statement : elseBody) {
    if (falseBranchReturns) {
      // Statements after the return!
      throw SlException(statement->line, statement->column,
                        "Code will never run");
    }
    statement->assignType(symbolTable, allGlobalSymbols);
    if (statement->returns) {
      falseBranchReturns = true;
    }
  }
  returns = trueBranchReturns && falseBranchReturns;
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

  staticlyConvert(value, **value->valueType, **valueType);
}
void CallNode::assignType(SymbolTable &symbolTable,
                          const SymbolTable &allGlobalSymbols) {
  function->assignType(symbolTable, allGlobalSymbols);
  if (!function->valueType) {
    throw SlException(line, column, "Function call has no type");
  }
  decayReferenceType(function);
  const auto &functionExpressionType = *function->valueType;
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
    decayReferenceType(arguments[i]);
    const auto &argumentType = *arguments[i]->valueType;
    staticlyConvert(arguments[i], *argumentType,
                    *functionTypeNode.arguments[i]);
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
void ReturnNode::assignType(SymbolTable &symbolTable,
                            const SymbolTable &allGlobalSymbols) {
  returns = true;
  value->assignType(symbolTable, allGlobalSymbols);
  if (!value->valueType) {
    throw SlException(line, column, "Return value has no type");
  }
  // We don't need to assign the type here because it is assigned during return
  // type checking, which happens when the function has finished type checking.
}
void ExternalNode::assignType(SymbolTable &symbolTable,
                              const SymbolTable &allGlobalSymbols) {
  // We already assigned our type in the constructor so we don't need to do
  // anything here.
}
void DereferenceNode::assignType(SymbolTable &symbolTable,
                                 const SymbolTable &allGlobalSymbols) {
  value->assignType(symbolTable, allGlobalSymbols);
  if (!value->valueType) {
    throw SlException(line, column, "Dereference expression has no type");
  }
  if (value->valueType->get()->type != TypeType::REFERENCE) {
    throw SlException(line, column,
                      "Dereference expression is not a reference");
  }
  const auto &referenceType =
      static_cast<const ReferenceTypeNode &>(**value->valueType);
  this->valueType = referenceType.type->clone();
}
void SubscriptNode::assignType(SymbolTable &symbolTable,
                               const SymbolTable &allGlobalSymbols) {
  value->assignType(symbolTable, allGlobalSymbols);
  if (!value->valueType) {
    throw SlException(line, column, "Subscript expression has no type");
  }
  decayReferenceType(value);
  const auto &valueType = *value->valueType;
  if (valueType->type != TypeType::ARRAY) {
    throw SlException(line, column, "Subscript expression is not an array");
  }
  const auto &arrayType = static_cast<const ArrayType &>(*valueType);
  index->assignType(symbolTable, allGlobalSymbols);
  if (!index->valueType) {
    throw SlException(line, column, "Subscript index has no type");
  }
  decayReferenceType(index);
  const auto &indexType = *index->valueType;
  if (!isIntegral(*indexType)) {
    throw SlException(line, column, "Subscript index is not an integer");
  }
  this->valueType =
      std::make_unique<ReferenceTypeNode>(arrayType.type->clone(), false);
}
} // namespace sl
