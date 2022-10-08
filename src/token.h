#ifndef SL_TOKEN_H
#define SL_TOKEN_H

#include <string>

namespace sl {
enum class TokenType {
  // Meta-tokens
  END,
  ERROR,
  // Literals
  IDENTIFIER,
  INTEGER,
  FLOAT,
  STRING,
  // Keywords
  LET,
  MUT,
  FN,
  IF,
  ELSE,
  WHILE,
  FOR,
  RETURN,
  // Operators
  EQUALS,
  PLUS,
  MINUS,
  STAR,
  SLASH,
  PERCENT,
  AMPERSAND,
  PIPE,
  CARET,
  TILDE,
  BANG,
  QUESTION,
  COLON,
  DOT,
  COMMA,
  SEMICOLON,
  LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  LEFT_BRACKET,
  RIGHT_BRACKET,
};

struct Token {
  TokenType type;
  std::string value;
  int line;
  int column;
};
}

#endif