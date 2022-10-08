#ifndef SL_AST_H
#define SL_AST_H

#include <memory>
#include <vector>

namespace sl {
enum class AstNodeType { COMPILATION_UNIT };
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
} // namespace sl

#endif