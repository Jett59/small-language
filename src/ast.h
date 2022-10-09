#ifndef SL_AST_H
#define SL_AST_H

#include "type.h"
#include <cstdlib>
#include <memory>
#include <string>
#include <vector>

namespace sl {
enum class AstNodeType {
  COMPILATION_UNIT,
  DEFINITION,
  INTEGER_LITERAL,
  FLOAT_LITERAL,
  STRING_LITERAL,
  FUNCTION,
};
class AstNode {
public:
  AstNodeType type;
  AstNode(AstNodeType type) : type(type) {}
  virtual ~AstNode() = default;
};
class CompilationUnitNode : public AstNode {
public:
  std::vector<std::unique_ptr<AstNode>> definitions;
  CompilationUnitNode(std::vector<std::unique_ptr<AstNode>> definitions)
      : AstNode(AstNodeType::COMPILATION_UNIT),
        definitions(std::move(definitions)) {}
};
class DefinitionNode : public AstNode {
public:
  bool constant;
  std::string name;
  std::unique_ptr<AstNode> initializer;
  DefinitionNode(std::string name, std::unique_ptr<AstNode> initializer,
                 const std::string &declarationKeyword)
      : AstNode(AstNodeType::DEFINITION), name(std::move(name)),
        initializer(std::move(initializer)),
        constant(declarationKeyword == "let") {}
};
class IntegerLiteralNode : public AstNode {
public:
  int64_t value;
  IntegerLiteralNode(int64_t value)
      : AstNode(AstNodeType::INTEGER_LITERAL), value(value) {}
  IntegerLiteralNode(const std::string &value)
      : AstNode(AstNodeType::INTEGER_LITERAL), value(std::stoll(value)) {}
};
class FloatLiteralNode : public AstNode {
public:
  double value;
  FloatLiteralNode(double value)
      : AstNode(AstNodeType::FLOAT_LITERAL), value(value) {}
  FloatLiteralNode(const std::string &value)
      : AstNode(AstNodeType::FLOAT_LITERAL), value(std::stod(value)) {}
};
class StringLiteralNode : public AstNode {
public:
  std::string value;
  StringLiteralNode(std::string value)
      : AstNode(AstNodeType::STRING_LITERAL), value(std::move(value)) {}
};
class FunctionNode : public AstNode {
public:
  std::unique_ptr<Type> returnType;
  std::vector<NameAndType> parameters;
  std::unique_ptr<AstNode> body;
  FunctionNode(std::unique_ptr<Type> returnType,
               std::vector<NameAndType> parameters,
               std::unique_ptr<AstNode> body)
      : AstNode(AstNodeType::FUNCTION), returnType(std::move(returnType)),
        parameters(std::move(parameters)), body(std::move(body)) {}
};
} // namespace sl

#endif