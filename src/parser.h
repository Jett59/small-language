#ifndef SL_PARSER_H
#define SL_PARSER_H

#include "ast.h"
#include "lexer.h"
#include <memory>

namespace sl {
enum class NonTerminal {
  COMPILATION_UNIT,
  EXPRESSION,
  INCOMPLETE_PARENNED_EXPRESSION_LIST,
  COMPLETE_PARENNED_EXPRESSION_LIST,
  STATEMENT,
  STATEMENT_LIST,
  TYPE,
  INCOMPLETE_PARENNED_TYPE_LIST,
  COMPLETE_PARENNED_TYPE_LIST,
  NAME_AND_TYPE,
  NAME_AND_TYPE_LIST
};
static inline std::string nonTerminalToString(NonTerminal nonTerminal) {
  switch (nonTerminal) {
  case NonTerminal::COMPILATION_UNIT:
    return "COMPILATION_UNIT";
  case NonTerminal::EXPRESSION:
    return "EXPRESSION";
  case NonTerminal::INCOMPLETE_PARENNED_EXPRESSION_LIST:
    return "INCOMPLETE_EXPRESSION_LIST";
  case NonTerminal::COMPLETE_PARENNED_EXPRESSION_LIST:
    return "COMPLETE_EXPRESSION_LIST";
  case NonTerminal::STATEMENT:
    return "STATEMENT";
  case NonTerminal::STATEMENT_LIST:
    return "STATEMENT_LIST";
  case NonTerminal::TYPE:
    return "TYPE";
  case NonTerminal::INCOMPLETE_PARENNED_TYPE_LIST:
    return "INCOMPLETE_TYPE_LIST";
  case NonTerminal::COMPLETE_PARENNED_TYPE_LIST:
    return "COMPLETE_TYPE_LIST";
  case NonTerminal::NAME_AND_TYPE:
    return "NAME_AND_TYPE";
  case NonTerminal::NAME_AND_TYPE_LIST:
    return "NAME_AND_TYPE_LIST";
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