#include "lexer.h"
#include <optional>

namespace sl {
char Lexer::readCharacter() {
  char c;
  if (bufferIndex >= buffer.length()) {
    do {
      input.read(&c, 1);
      if (input.eof()) {
        return '\0';
      }
    } while (c == '\r');
    buffer += c;
    bufferIndex++;
  } else {
    c = buffer[bufferIndex++];
  }
  if (c == '\n') {
    currentLocation.end.line++;
    currentLocation.end.column = 1;
  } else {
    currentLocation.end.column++;
  }
  return c;
}
void Lexer::unreadCharacter() {
  if (bufferIndex > 0) {
    if (buffer[bufferIndex] == '\n') {
      currentLocation.end.line--;
      currentLocation.end.column = 1;
    } else {
      currentLocation.end.column--;
    }
    bufferIndex--;
  }
}

using SymbolType = Parser::symbol_type;

static std::optional<SymbolType> getIdentifier(Lexer &lexer,
                                               location &location) {
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
  return Parser::make_IDENTIFIER(result, location);
}

static std::optional<SymbolType> getNumber(Lexer &lexer, location &location) {
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
    return Parser::make_INTEGER_LITERAL(result, location);
  }
  lexer.unreadCharacter();
  result.pop_back();
  return Parser::make_FLOAT_LITERAL(result, location);
}

static std::optional<SymbolType> getString(Lexer &lexer, location &location) {
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
  return Parser::make_STRING_LITERAL(result.substr(1, result.length() - 2),
                                     location);
}

using TokenGetter = std::optional<SymbolType> (*)(Lexer &, location &);

template <char c, SymbolType (*tokenConstructor)(location location)>
std::optional<SymbolType> singleCharacterTokenGetter(Lexer &lexer,
                                                     location &location) {
  char read = lexer.readCharacter();
  if (read != c) {
    lexer.unreadCharacter();
    return std::nullopt;
  }
  return tokenConstructor(location);
}

// Helper struct to allow passing literal strings to template parameters.
template <size_t size> struct StringLiteral {
  char value[size];
  constexpr StringLiteral(const char (&literal)[size]) {
    std::copy_n(literal, size, value);
  }

  constexpr char operator[](size_t i) const { return value[i]; }
};

template <StringLiteral string,
          SymbolType (*tokenConstructor)(location location)>
std::optional<SymbolType> keywordTokenGetter(Lexer &lexer, location &location) {
  for (int i = 0; string[i] != '\0'; i++) {
    char read = lexer.readCharacter();
    if (read != string[i]) {
      return std::nullopt;
    }
  }
  return tokenConstructor(location);
}

TokenGetter tokenGetters[] = {
    keywordTokenGetter<"let", Parser::make_LET>,
    keywordTokenGetter<"mut", Parser::make_MUT>,
    keywordTokenGetter<"fn", Parser::make_FN>,
    keywordTokenGetter<"if", Parser::make_IF>,
    keywordTokenGetter<"else", Parser::make_ELSE>,
    keywordTokenGetter<"while", Parser::make_WHILE>,
    keywordTokenGetter<"for", Parser::make_FOR>,
    keywordTokenGetter<"as", Parser::make_AS>,
    keywordTokenGetter<"return", Parser::make_RETURN>,
    keywordTokenGetter<"extern", Parser::make_EXTERN>,
    keywordTokenGetter<"true", Parser::make_TRUE>,
    keywordTokenGetter<"false", Parser::make_FALSE>,
    keywordTokenGetter<"nil", Parser::make_NIL>,
    keywordTokenGetter<"i8", Parser::make_I8>,
    keywordTokenGetter<"i16", Parser::make_I16>,
    keywordTokenGetter<"i32", Parser::make_I32>,
    keywordTokenGetter<"i64", Parser::make_I64>,
    keywordTokenGetter<"u8", Parser::make_U8>,
    keywordTokenGetter<"u16", Parser::make_U16>,
    keywordTokenGetter<"u32", Parser::make_U32>,
    keywordTokenGetter<"u64", Parser::make_U64>,
    keywordTokenGetter<"f32", Parser::make_F32>,
    keywordTokenGetter<"f64", Parser::make_F64>,
    keywordTokenGetter<"bool", Parser::make_BOOL>,
    keywordTokenGetter<"char", Parser::make_CHAR>,
    getIdentifier,
    getNumber,
    getString,
    keywordTokenGetter<"->", Parser::make_ARROW>,
    singleCharacterTokenGetter<'(', Parser::make_LEFT_PAREN>,
    singleCharacterTokenGetter<')', Parser::make_RIGHT_PAREN>,
    singleCharacterTokenGetter<'{', Parser::make_LEFT_BRACE>,
    singleCharacterTokenGetter<'}', Parser::make_RIGHT_BRACE>,
    singleCharacterTokenGetter<'[', Parser::make_LEFT_BRACKET>,
    singleCharacterTokenGetter<']', Parser::make_RIGHT_BRACKET>,
    singleCharacterTokenGetter<',', Parser::make_COMMA>,
    singleCharacterTokenGetter<'.', Parser::make_DOT>,
    singleCharacterTokenGetter<'-', Parser::make_MINUS>,
    singleCharacterTokenGetter<'+', Parser::make_PLUS>,
    singleCharacterTokenGetter<';', Parser::make_SEMICOLON>,
    singleCharacterTokenGetter<'/', Parser::make_SLASH>,
    singleCharacterTokenGetter<'*', Parser::make_STAR>,
    keywordTokenGetter<"!=", Parser::make_BANG_EQUALS>,
    singleCharacterTokenGetter<'!', Parser::make_BANG>,
    keywordTokenGetter<"==", Parser::make_EQUALS_EQUALS>,
    singleCharacterTokenGetter<'=', Parser::make_EQUALS>,
    keywordTokenGetter<"<=", Parser::make_LESS_EQUALS>,
    singleCharacterTokenGetter<'<', Parser::make_LESS>,
    keywordTokenGetter<">=", Parser::make_GREATER_EQUALS>,
    singleCharacterTokenGetter<'>', Parser::make_GREATER>,
    singleCharacterTokenGetter<'%', Parser::make_PERCENT>,
    keywordTokenGetter<"&&", Parser::make_AMPERSAND_AMPERSAND>,
    singleCharacterTokenGetter<'&', Parser::make_AMPERSAND>,
    keywordTokenGetter<"||", Parser::make_PIPE_PIPE>,
    singleCharacterTokenGetter<'|', Parser::make_PIPE>,
    singleCharacterTokenGetter<'^', Parser::make_CARET>,
    singleCharacterTokenGetter<'~', Parser::make_TILDE>,
    singleCharacterTokenGetter<'?', Parser::make_QUESTION>,
    singleCharacterTokenGetter<':', Parser::make_COLON>,
};

SymbolType Lexer::nextToken() {
  char c;
  do {
    c = readCharacter();
    if (input.eof()) {
      return Parser::make_YYEOF(currentLocation);
    }
  } while (isspace(c));
  unreadCharacter();
  currentLocation.begin =
      currentLocation.end; // The previous end is the new start.
  location startLocation = currentLocation;
  size_t startBufferIndex = bufferIndex;
  SymbolType bestMatch;
  size_t bestFinalColumn = 0;
  size_t bestFinalLine = 0;
  size_t bestFinalBufferIndex = 0;
  for (auto getter : tokenGetters) {
    auto token = getter(*this, currentLocation);
    if (token) {
      size_t line = currentLocation.end.line;
      size_t column = currentLocation.end.column;
      if (bestMatch.empty() || line > bestFinalLine ||
          column > bestFinalColumn) {
        bestMatch.move(*token);
        bestFinalColumn = column;
        bestFinalLine = line;
        bestFinalBufferIndex = bufferIndex;
      }
    }
    bufferIndex = startBufferIndex;
    currentLocation = startLocation;
  }
  if (!bestMatch.empty()) {
    currentLocation = startLocation;
    currentLocation.end.line = bestFinalLine;
    currentLocation.end.column = bestFinalColumn;
    bufferIndex = bestFinalBufferIndex;
    return bestMatch;
  }
  if (input.eof()) {
    return Parser::make_YYEOF(currentLocation);
  } else {
    return Parser::make_YYUNDEF(currentLocation);
  }
}
} // namespace sl
