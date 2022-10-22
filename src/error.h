#ifndef SL_ERROR_H
#define SL_ERROR_H

#include <format>
#include <string>

namespace sl {
class SlException {
public:
  SlException(int line, int column, std::string message)
      : line(line), column(column), message(message) {}
  std::string what() const noexcept {
    return std::format("{}:{}: {}", line, column, message);
  }

private:
  std::string message;
  int line, column;
};
} // namespace sl

#endif