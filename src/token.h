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
  BOOL_LITERAL,
  LET,
  MUT,
  FN,
  IF,
  ELSE,
  WHILE,
  FOR,
  AS,
  RETURN,
  EXTERN,
  NIL,
  I8,
  I16,
  I32,
  I64,
  U8,
  U16,
  U32,
  U64,
  F32,
  F64,
  BOOL,
  CHAR,
  STRING_TYPE,
  EQUALS,
  EQUALS_EQUALS,
  BANG_EQUALS,
  LESS,
  LESS_EQUALS,
  GREATER,
  GREATER_EQUALS,
  PLUS,
  MINUS,
  STAR,
  SLASH,
  PERCENT,
  AMPERSAND,
  AMPERSAND_AMPERSAND,
  PIPE,
  PIPE_PIPE,
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
  ARROW,
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
  case TokenType::BOOL_LITERAL:
    return "bool literal";
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
  case TokenType::AS:
    return "as";
  case TokenType::RETURN:
    return "return";
  case TokenType::EXTERN:
    return "extern";
  case TokenType::NIL:
    return "nil";
  case TokenType::I8:
    return "i8";
  case TokenType::I16:
    return "i16";
  case TokenType::I32:
    return "i32";
  case TokenType::I64:
    return "i64";
  case TokenType::U8:
    return "u8";
  case TokenType::U16:
    return "u16";
  case TokenType::U32:
    return "u32";
  case TokenType::U64:
    return "u64";
  case TokenType::F32:
    return "f32";
  case TokenType::F64:
    return "f64";
  case TokenType::BOOL:
    return "bool";
  case TokenType::CHAR:
    return "char";
  case TokenType::STRING_TYPE:
    return "string";
  case TokenType::EQUALS:
    return "=";
  case TokenType::EQUALS_EQUALS:
    return "==";
  case TokenType::BANG_EQUALS:
    return "!=";
  case TokenType::LESS:
    return "<";
  case TokenType::LESS_EQUALS:
    return "<=";
  case TokenType::GREATER:
    return ">";
  case TokenType::GREATER_EQUALS:
    return ">=";
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
  case TokenType::AMPERSAND_AMPERSAND:
    return "&&";
  case TokenType::PIPE:
    return "|";
  case TokenType::PIPE_PIPE:
    return "||";
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
  case TokenType::ARROW:
    return "->";
  }
  return "unknown";
}
} // namespace sl

#endif