#ifndef SL_AST_H
#define SL_AST_H

#include "type.h"
#include <cstdlib>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace sl {
enum class AstNodeType {
  COMPILATION_UNIT,
  DEFINITION,
  INTEGER_LITERAL,
  FLOAT_LITERAL,
  STRING_LITERAL,
  BOOL_LITERAL,
  ARRAY_LITERAL,
  FUNCTION,
  BINARY_OPERATOR,
  NIL,
  IF_STATEMENT,
  VARIABLE_REFERENCE,
  CAST,
  CALL,
  RETURN,
  EXTERNAL,
  DEREFERENCE,
  SUBSCRIPT,
};

// Forward-declare to allow for use in the symbol table.
class DefinitionNode;

class AstNode {
public:
  AstNodeType type;
  std::optional<std::unique_ptr<Type>> valueType;
  int line = 0, column = 0;
  bool returns = false;

  AstNode(AstNodeType type) : type(type) {}
  virtual ~AstNode() = default;

  virtual std::string toString() const = 0;

  using SymbolTable = std::map<std::string, const DefinitionNode &>;

  virtual void assignType(SymbolTable &symbolTable,
                          const SymbolTable &allGlobalSymbols) = 0;
};
class CompilationUnitNode : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> definitions;
  CompilationUnitNode(std::vector<std::unique_ptr<AstNode>> definitions)
      : AstNode(AstNodeType::COMPILATION_UNIT),
        definitions(std::move(definitions)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class DefinitionNode : public AstNode {
public:
  bool constant;
  std::string name;
  std::unique_ptr<AstNode> initializer;
  DefinitionNode(std::string name, std::unique_ptr<AstNode> initializer,
                 bool constant,
                 std::optional<std::unique_ptr<Type>> valueType = std::nullopt)
      : AstNode(AstNodeType::DEFINITION), name(std::move(name)),
        initializer(std::move(initializer)), constant(constant) {
    if (valueType) {
      this->valueType = std::move(*valueType);
    }
  }

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
static PrimitiveType getSmallestTypeThatContains(int64_t value) {
  if (value >= std::numeric_limits<int8_t>::min() &&
      value <= std::numeric_limits<int8_t>::max()) {
    return PrimitiveType::I8;
  } else if (value >= std::numeric_limits<int16_t>::min() &&
             value <= std::numeric_limits<int16_t>::max()) {
    return PrimitiveType::I16;
  } else if (value >= std::numeric_limits<int32_t>::min() &&
             value <= std::numeric_limits<int32_t>::max()) {
    return PrimitiveType::I32;
  } else {
    return PrimitiveType::I64;
  }
}

class IntegerLiteralNode : public AstNode {
public:
  int64_t value;
  IntegerLiteralNode(int64_t value)
      : AstNode(AstNodeType::INTEGER_LITERAL), value(value) {}
  IntegerLiteralNode(const std::string &value)
      : AstNode(AstNodeType::INTEGER_LITERAL), value(std::stoll(value)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class FloatLiteralNode : public AstNode {
public:
  double value;
  FloatLiteralNode(double value)
      : AstNode(AstNodeType::FLOAT_LITERAL), value(value) {}
  FloatLiteralNode(const std::string &value)
      : AstNode(AstNodeType::FLOAT_LITERAL), value(std::stod(value)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class StringLiteralNode : public AstNode {
public:
  std::string value;
  StringLiteralNode(std::string value)
      : AstNode(AstNodeType::STRING_LITERAL), value(std::move(value)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class BooleanLiteralNode : public AstNode {
public:
  bool value;
  BooleanLiteralNode(bool value)
      : AstNode(AstNodeType::BOOL_LITERAL), value(value) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class ArrayLiteralNode : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> values;
  ArrayLiteralNode(std::vector<std::unique_ptr<AstNode>> values)
      : AstNode(AstNodeType::ARRAY_LITERAL), values(std::move(values)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class FunctionNode : public AstNode {
public:
  std::unique_ptr<Type> returnType;
  std::vector<std::unique_ptr<NameAndType>> parameters;
  std::vector<std::unique_ptr<AstNode>> body;
  FunctionNode(std::unique_ptr<Type> returnType,
               std::vector<std::unique_ptr<NameAndType>> parameters,
               std::vector<std::unique_ptr<AstNode>> body)
      : AstNode(AstNodeType::FUNCTION), returnType(std::move(returnType)),
        parameters(std::move(parameters)), body(std::move(body)) {}
  FunctionNode(std::unique_ptr<Type> returnType,
               std::vector<std::unique_ptr<AstNode>> body)
      : AstNode(AstNodeType::FUNCTION), returnType(std::move(returnType)),
        body(std::move(body)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
enum class BinaryOperatorType {
  ADD,
  SUBTRACT,
  MULTIPLY,
  DIVIDE,
  MODULO,
  EQUAL,
  NOT_EQUAL,
  LESS_THAN,
  LESS_THAN_OR_EQUAL,
  GREATER_THAN,
  GREATER_THAN_OR_EQUAL,
  LOGICAL_OR,
  LOGICAL_AND,
  BITWISE_OR,
  BITWISE_AND,
  BITWISE_XOR,
  BITWISE_SHIFT_LEFT,
  BITWISE_SHIFT_RIGHT,
  ASSIGN,
};
static std::map<BinaryOperatorType, std::string> binaryOperatorTypeToString = {
    {BinaryOperatorType::ADD, "+"},
    {BinaryOperatorType::SUBTRACT, "-"},
    {BinaryOperatorType::MULTIPLY, "*"},
    {BinaryOperatorType::DIVIDE, "/"},
    {BinaryOperatorType::MODULO, "%"},
    {BinaryOperatorType::EQUAL, "=="},
    {BinaryOperatorType::NOT_EQUAL, "!="},
    {BinaryOperatorType::LESS_THAN, "<"},
    {BinaryOperatorType::LESS_THAN_OR_EQUAL, "<="},
    {BinaryOperatorType::GREATER_THAN, ">"},
    {BinaryOperatorType::GREATER_THAN_OR_EQUAL, ">="},
    {BinaryOperatorType::LOGICAL_OR, "||"},
    {BinaryOperatorType::LOGICAL_AND, "&&"},
    {BinaryOperatorType::BITWISE_OR, "|"},
    {BinaryOperatorType::BITWISE_AND, "&"},
    {BinaryOperatorType::BITWISE_XOR, "^"},
    {BinaryOperatorType::BITWISE_SHIFT_LEFT, "<<"},
    {BinaryOperatorType::BITWISE_SHIFT_RIGHT, ">>"},
    {BinaryOperatorType::ASSIGN, "="},
};

class BinaryOperatorNode : public AstNode {
public:
  BinaryOperatorType operatorType;
  std::unique_ptr<AstNode> left;
  std::unique_ptr<AstNode> right;
  std::unique_ptr<Type> operandType;
  BinaryOperatorNode(BinaryOperatorType operatorType,
                     std::unique_ptr<AstNode> left,
                     std::unique_ptr<AstNode> right)
      : AstNode(AstNodeType::BINARY_OPERATOR), operatorType(operatorType),
        left(std::move(left)), right(std::move(right)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class NilNode : public AstNode {
public:
  NilNode() : AstNode(AstNodeType::NIL) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class IfStatementNode : public AstNode {
public:
  std::unique_ptr<AstNode> condition;
  std::vector<std::unique_ptr<AstNode>> thenBody;
  std::vector<std::unique_ptr<AstNode>> elseBody;
  IfStatementNode(std::unique_ptr<AstNode> condition,
                  std::vector<std::unique_ptr<AstNode>> thenBody,
                  std::vector<std::unique_ptr<AstNode>> elseBody = {})
      : AstNode(AstNodeType::IF_STATEMENT), condition(std::move(condition)),
        thenBody(std::move(thenBody)), elseBody(std::move(elseBody)) {}
  IfStatementNode(std::unique_ptr<AstNode> condition,
                  std::vector<std::unique_ptr<AstNode>> thenBody,
                  std::unique_ptr<AstNode> elseStatement)
      : AstNode(AstNodeType::IF_STATEMENT), condition(std::move(condition)),
        thenBody(std::move(thenBody)) {
    elseBody.push_back(std::move(elseStatement));
  }

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class VariableReferenceNode : public AstNode {
public:
  std::string name;
  VariableReferenceNode(std::string name)
      : AstNode(AstNodeType::VARIABLE_REFERENCE), name(std::move(name)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class CastNode : public AstNode {
public:
  std::unique_ptr<AstNode> value;
  CastNode(std::unique_ptr<AstNode> value, std::unique_ptr<Type> type)
      : AstNode(AstNodeType::CAST), value(std::move(value)) {
    this->valueType = std::move(type);
  }

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class CallNode : public AstNode {
public:
  std::unique_ptr<AstNode> function;
  std::vector<std::unique_ptr<AstNode>> arguments;
  CallNode(std::unique_ptr<AstNode> function,
           std::vector<std::unique_ptr<AstNode>> arguments = {})
      : AstNode(AstNodeType::CALL), function(std::move(function)),
        arguments(std::move(arguments)) {}
  CallNode(std::unique_ptr<AstNode> function,
           std::unique_ptr<AstNode> singleArgument)
      : AstNode(AstNodeType::CALL), function(std::move(function)) {
    arguments.push_back(std::move(singleArgument));
  }

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class ReturnNode : public AstNode {
public:
  std::unique_ptr<AstNode> value;
  ReturnNode(std::unique_ptr<AstNode> value)
      : AstNode(AstNodeType::RETURN), value(std::move(value)) {}
  ReturnNode() : AstNode(AstNodeType::RETURN) {
    value = std::make_unique<NilNode>();
  }

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class ExternalNode : public AstNode {
public:
  std::string name;
  ExternalNode(std::unique_ptr<NameAndType> nameAndType)
      : AstNode(AstNodeType::EXTERNAL), name(std::move(nameAndType->name)) {
    valueType =
        std::make_unique<ReferenceTypeNode>(std::move(nameAndType->type), true);
  }

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class DereferenceNode : public AstNode {
public:
  std::unique_ptr<AstNode> value;
  DereferenceNode(std::unique_ptr<AstNode> value)
      : AstNode(AstNodeType::DEREFERENCE), value(std::move(value)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
class SubscriptNode : public AstNode {
public:
  std::unique_ptr<AstNode> value;
  std::unique_ptr<AstNode> index;
  SubscriptNode(std::unique_ptr<AstNode> value, std::unique_ptr<AstNode> index)
      : AstNode(AstNodeType::SUBSCRIPT), value(std::move(value)),
        index(std::move(index)) {}

  std::string toString() const override;

  void assignType(SymbolTable &symbolTable,
                  const SymbolTable &allGlobalSymbols) override;
};
} // namespace sl

#endif