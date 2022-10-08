#ifndef SL_PARSER_H
#define SL_PARSER_H

#include "ast.h"
#include "lexer.h"
#include <memory>

namespace sl {
enum class NonTerminal {
  COMPILATION_UNIT,
  DEFINITIONS,
  DEFINITION,
  EXPRESSION,
  FUNCTION_EXPRESSION,
  INT_EXPRESSION,
  FLOAT_EXPRESSION,
  STRING_EXPRESSION,
  VARIABLE_REFERENCE,
  STATEMENT,
  STATEMENT_LIST,
  RETURN_STATEMENT,
  EXPRESSION_STATEMENT,
};
class Parser {
public:
  Parser(Lexer &lexer) : lexer(lexer) {}
  std::unique_ptr<AstNode> parse();

private:
  Lexer &lexer;
};
} // namespace sl

#endif