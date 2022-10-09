#ifndef SL_PARSER_H
#define SL_PARSER_H

#include "ast.h"
#include "lexer.h"
#include <memory>

namespace sl {
enum class NonTerminal {
  COMPILATION_UNIT,
  EXPRESSION,
  STATEMENT,
  STATEMENT_LIST,
};
static inline std::string nonTerminalToString(NonTerminal nonTerminal) {
  switch (nonTerminal) {
  case NonTerminal::COMPILATION_UNIT:
    return "COMPILATION_UNIT";
  case NonTerminal::EXPRESSION:
    return "EXPRESSION";
  case NonTerminal::STATEMENT:
    return "STATEMENT";
  case NonTerminal::STATEMENT_LIST:
    return "STATEMENT_LIST";
  }
  return "UNKNOWN";
}
class Parser {
public:
  Parser(Lexer &lexer) : lexer(lexer) {}
  std::unique_ptr<AstNode> parse();

private:
  Lexer &lexer;
};
} // namespace sl

#endif