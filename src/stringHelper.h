#ifndef SL_STRING_HELPER_H
#define SL_STRING_HELPER_H

#include <stdexcept>
#include <string>

namespace sl {
static inline std::string translateEscapes(const std::string &str) {
  std::string result;
  for (size_t i = 0; i < str.size(); i++) {
    if (str[i] == '\\') {
      i++;
      if (i >= str.size()) {
        break;
      }
      switch (str[i]) {
      case 'a':
        result += '\a';
        break;
      case 'b':
        result += '\b';
        break;
      case 'f':
        result += '\f';
        break;
      case 'n':
        result += '\n';
        break;
      case 'r':
        result += '\r';
        break;
      case 't':
        result += '\t';
        break;
      case 'v':
        result += '\v';
        break;
      case '\\':
        result += '\\';
        break;
      case '"':
        result += '"';
        break;
      default:
        throw std::runtime_error("Unknown escape sequence: \\" + str[i]);
      }
    } else {
      result += str[i];
    }
  }
  return result;
}
} // namespace sl

#endif