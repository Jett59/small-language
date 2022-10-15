#include "lexer.h"
#include "token.h"
#include <optional>

namespace sl {
char Lexer::readCharacter() {
  char c;
  if (bufferIndex >= buffer.length()) {
    do {
      input.read(&c, 1);
    } while (c == '\r');
    if (c == '\n') {
      line++;
      column = 1;
    } else {
      column++;
    }
    buffer += c;
    bufferIndex++;
  } else {
    c = buffer[bufferIndex++];
  }
  return c;
}
void Lexer::unreadCharacter() {
  if (bufferIndex > 0) {
    bufferIndex--;
    if (buffer[bufferIndex] == '\n') {
      line--;
      column = 1;
    } else {
      column--;
    }
  }
}
static std::optional<Token> getIdentifier(Lexer &lexer) {
  std::string result;
  char c = lexer.readCharacter();
  if (!isalpha(c)) {
    lexer.unreadCharacter();
    return std::nullopt;
  }
  result += c;
  do {
    c = lexer.readCharacter();
    result += c;
  } while (isalnum(c));
  lexer.unreadCharacter();
  result.pop_back();
  return Token{.type = TokenType::IDENTIFIER, .value = result};
}

static std::optional<Token> getNumber(Lexer &lexer) {
  std::string result;
  char c = lexer.readCharacter();
  if (!isdigit(c)) {
    lexer.unreadCharacter();
    return std::nullopt;
  }
  result += c;
  do {
    c = lexer.readCharacter();
    result += c;
  } while (isdigit(c));
  if (c == '.') {
    do {
      c = lexer.readCharacter();
      result += c;
    } while (isdigit(c));
  } else {
    lexer.unreadCharacter();
    result.pop_back();
    return Token{.type = TokenType::INTEGER, .value = result};
  }
  lexer.unreadCharacter();
  result.pop_back();
  return Token{.type = TokenType::FLOAT, .value = result};
}

static std::optional<Token> getString(Lexer &lexer) {
  std::string result;
  char c = lexer.readCharacter();
  if (c != '"') {
    lexer.unreadCharacter();
    return std::nullopt;
  }
  result += c;
  do {
    c = lexer.readCharacter();
    result += c;
    if (c == '\\') {
      // Ignore the next character to handle "\"".
      c = lexer.readCharacter();
      result += c;
      c = lexer.readCharacter();
      result += c;
    }
  } while (c != '"');
  return Token{.type = TokenType::STRING, .value = result};
}

using TokenGetter = std::optional<Token> (*)(Lexer &);

template <TokenType tokenType, char c>
std::optional<Token> singleCharacterTokenGetter(Lexer &lexer) {
  char read = lexer.readCharacter();
  if (read != c) {
    lexer.unreadCharacter();
    return std::nullopt;
  }
  return Token{.type = tokenType, .value = std::string(1, c)};
}

// Helper struct to allow passing literal strings to template parameters.
template <size_t size> struct StringLiteral {
  char value[size];
  constexpr StringLiteral(const char (&literal)[size]) {
    std::copy_n(literal, size, value);
  }

  constexpr char operator[](size_t i) const {
    return value[i];
  }
};

template <TokenType tokenType, StringLiteral string>
std::optional<Token> keywordTokenGetter(Lexer &lexer) {
  for (int i = 0; string[i] != '\0'; i++) {
    char read = lexer.readCharacter();
    if (read != string[i]) {
      return std::nullopt;
    }
  }
  return Token{.type = tokenType, .value = std::string(string.value)};
}

TokenGetter tokenGetters[] = {
    keywordTokenGetter<TokenType::LET, "let">,
    keywordTokenGetter<TokenType::MUT, "mut">,
    keywordTokenGetter<TokenType::FN, "fn">,
    keywordTokenGetter<TokenType::IF, "if">,
    keywordTokenGetter<TokenType::ELSE, "else">,
    keywordTokenGetter<TokenType::WHILE, "while">,
    keywordTokenGetter<TokenType::FOR, "for">,
    keywordTokenGetter<TokenType::NIL, "nil">,
    keywordTokenGetter<TokenType::I8, "i8">,
    keywordTokenGetter<TokenType::I16, "i16">,
    keywordTokenGetter<TokenType::I32, "i32">,
    keywordTokenGetter<TokenType::I64, "i64">,
    keywordTokenGetter<TokenType::U8, "u8">,
    keywordTokenGetter<TokenType::U16, "u16">,
    keywordTokenGetter<TokenType::U32, "u32">,
    keywordTokenGetter<TokenType::U64, "u64">,
    keywordTokenGetter<TokenType::F32, "f32">,
    keywordTokenGetter<TokenType::F64, "f64">,
    keywordTokenGetter<TokenType::BOOL, "bool">,
    keywordTokenGetter<TokenType::CHAR, "char">,
    keywordTokenGetter<TokenType::STRING_TYPE, "string">,
    getIdentifier,
    getNumber,
    getString,
    keywordTokenGetter<TokenType::ARROW, "->">,
    singleCharacterTokenGetter<TokenType::LEFT_PAREN, '('>,
    singleCharacterTokenGetter<TokenType::RIGHT_PAREN, ')'>,
    singleCharacterTokenGetter<TokenType::LEFT_BRACE, '{'>,
    singleCharacterTokenGetter<TokenType::RIGHT_BRACE, '}'>,
    singleCharacterTokenGetter<TokenType::COMMA, ','>,
    singleCharacterTokenGetter<TokenType::DOT, '.'>,
    singleCharacterTokenGetter<TokenType::MINUS, '-'>,
    singleCharacterTokenGetter<TokenType::PLUS, '+'>,
    singleCharacterTokenGetter<TokenType::SEMICOLON, ';'>,
    singleCharacterTokenGetter<TokenType::SLASH, '/'>,
    singleCharacterTokenGetter<TokenType::STAR, '*'>,
    singleCharacterTokenGetter<TokenType::BANG, '!'>,
    singleCharacterTokenGetter<TokenType::EQUALS, '='>,
    keywordTokenGetter<TokenType::EQUALS_EQUALS, "==">,
    keywordTokenGetter<TokenType::BANG_EQUALS, "!=">,
    singleCharacterTokenGetter<TokenType::LESS, '<'>,
    keywordTokenGetter<TokenType::LESS_EQUALS, "<=">,
    singleCharacterTokenGetter<TokenType::GREATER, '>'>,
    keywordTokenGetter<TokenType::GREATER_EQUALS, ">=">,
    singleCharacterTokenGetter<TokenType::PERCENT, '%'>,
    singleCharacterTokenGetter<TokenType::AMPERSAND, '&'>,
    keywordTokenGetter<TokenType::AMPERSAND_AMPERSAND, "&&">,
    singleCharacterTokenGetter<TokenType::PIPE, '|'>,
    keywordTokenGetter<TokenType::PIPE_PIPE, "||">,
    singleCharacterTokenGetter<TokenType::CARET, '^'>,
    singleCharacterTokenGetter<TokenType::TILDE, '~'>,
    singleCharacterTokenGetter<TokenType::QUESTION, '?'>,
    singleCharacterTokenGetter<TokenType::COLON, ':'>,
};

Token Lexer::nextToken() {
  char c;
  do {
    c = readCharacter();
    if (input.eof()) {
      return Token{.type = TokenType::END, .value = ""};
    }
  } while (isspace(c));
  unreadCharacter();
  int startLine = line, startColumn = column;
  size_t startBufferIndex = bufferIndex;
  for (auto getter : tokenGetters) {
    auto token = getter(*this);
    if (token) {
      buffer = buffer.substr(bufferIndex);
      bufferIndex = 0;
      token->line = startLine;
      token->column = startColumn;
      return *token;
    }
    bufferIndex = startBufferIndex;
    line = startLine;
    column = startColumn;
  }
  if (input.eof()) {
    return Token{.type = TokenType::END};
  } else {
    return Token{.type = TokenType::ERROR, .value = std::string(1, c)};
  }
}
} // namespace sl
