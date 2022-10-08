#ifndef SL_TOKEN_H
#define SL_TOKEN_H

#include <string>

namespace sl {
enum class TokenType {
  END,
  ERROR,
  IDENTIFIER,
  INTEGER,
  FLOAT,
  STRING,
  LET,
  MUT,
  FN,
  IF,
  ELSE,
  WHILE,
  FOR,
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

static inline std::string tokenTypeToString(TokenType tokenType) {
  switch (tokenType) {
  case TokenType::END:
    return "end of file";
  case TokenType::ERROR:
    return "error";
  case TokenType::IDENTIFIER:
    return "identifier";
  case TokenType::INTEGER:
    return "integer";
  case TokenType::FLOAT:
    return "float";
  case TokenType::STRING:
    return "string";
  case TokenType::LET:
    return "let";
  case TokenType::MUT:
    return "mut";
  case TokenType::FN:
    return "fn";
  case TokenType::IF:
    return "if";
  case TokenType::ELSE:
    return "else";
  case TokenType::WHILE:
    return "while";
  case TokenType::FOR:
    return "for";
  case TokenType::EQUALS:
    return "=";
  case TokenType::PLUS:
    return "+";
  case TokenType::MINUS:
    return "-";
  case TokenType::STAR:
    return "*";
  case TokenType::SLASH:
    return "/";
  case TokenType::PERCENT:
    return "%";
  case TokenType::AMPERSAND:
    return "&";
  case TokenType::PIPE:
    return "|";
  case TokenType::CARET:
    return "^";
  case TokenType::TILDE:
    return "~";
  case TokenType::BANG:
    return "!";
  case TokenType::QUESTION:
    return "?";
  case TokenType::COLON:
    return ":";
  case TokenType::DOT:
    return ".";
  case TokenType::COMMA:
    return ",";
  case TokenType::SEMICOLON:
    return ";";
  case TokenType::LEFT_PAREN:
    return "(";
  case TokenType::RIGHT_PAREN:
    return ")";
  case TokenType::LEFT_BRACE:
    return "{";
  case TokenType::RIGHT_BRACE:
    return "}";
  case TokenType::LEFT_BRACKET:
    return "[";
  case TokenType::RIGHT_BRACKET:
    return "]";
  }
  return "unknown";
}
}

#endif