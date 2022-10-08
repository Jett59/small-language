#include "lexer.h"
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

using TokenGetter = std::optional<Token> (*)(Lexer &);

TokenGetter tokenGetters[] = {
    getIdentifier,
};

Token Lexer::nextToken() {
  char c;
  do {
    c = readCharacter();
  } while (isspace(c));
  unreadCharacter();
  int startLine = line, startColumn = column;
  size_t startBufferIndex = bufferIndex;
  for (auto getter : tokenGetters) {
    auto token = getter(*this);
    if (token) {
        buffer = buffer.substr(bufferIndex);
        token->line = startLine;
        token->column = startColumn;
      return *token;
    }
    bufferIndex = startBufferIndex;
  }
  return Token{.type = TokenType::ERROR, .value = std::string(1, c)};
}
} // namespace sl
