#ifndef SL_LEXER_H
#define SL_LEXER_H

#include "token.h"
#include <algorithm>
#include <cstdint>
#include <fstream>

namespace sl {
class Lexer {
public:
  Lexer(std::string fileName, std::istream &input)
      : fileName(std::move(fileName)), input(input) {}

  Token nextToken();

  char readCharacter();
  void unreadCharacter();

private:
  std::string fileName;
  std::istream &input;

  std::string buffer;
  size_t bufferIndex = 0;
  int line = 1, column = 1;
};
} // namespace sl

#endif