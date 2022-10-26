#ifndef SL_LEXER_H
#define SL_LEXER_H

#include "parser.hh"
#include <algorithm>
#include <cstdint>
#include <fstream>

namespace sl {
class Lexer {
public:
  Lexer(std::string fileName, std::istream &input)
      : fileName(std::move(fileName)), input(input) {
    currentLocation.initialize();
  }

  Parser::symbol_type nextToken();

  char readCharacter();
  void unreadCharacter();

private:
  std::string fileName;
  std::istream &input;

  std::string buffer;
  size_t bufferIndex = 0;
  location currentLocation;
};
} // namespace sl

#endif