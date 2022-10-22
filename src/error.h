#ifndef SL_ERROR_H
#define SL_ERROR_H

#include <string>

namespace sl {
class SlException {
public:
  SlException(int line, int column, std::string message)
      : line(line), column(column), message(message) {}
  std::string what() const noexcept {
    return std::to_string(line) + ":" + std::to_string(column) + ": " + message;
  }

private:
  std::string message;
  int line, column;
};
} // namespace sl

#endif