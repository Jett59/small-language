#include "parser.h"
#include <memory>
#include <variant>
#include <vector>

namespace sl {
using std::string_literals::operator""s;

using SymbolType = std::variant<TokenType, NonTerminal>;

struct ParserSymbol {
  SymbolType type;
  std::variant<nullptr_t, std::unique_ptr<AstNode>,
               std::vector<std::unique_ptr<AstNode>>, std::string>
      value;
};

enum class Precedence {
  NONE,
  ASSIGNMENT,
  LOGICAL,
  COMPARISON,
  SUM,
  PRODUCT,
  BITWISE,
  PREFIX,
  POSTFIX,
};

static inline bool operator<(Precedence a, Precedence b) {
  return static_cast<int>(a) < static_cast<int>(b);
}
static inline bool operator<=(Precedence a, Precedence b) {
  return static_cast<int>(a) <= static_cast<int>(b);
}
static inline bool operator>(Precedence a, Precedence b) {
  return static_cast<int>(a) > static_cast<int>(b);
}
static inline bool operator>=(Precedence a, Precedence b) {
  return static_cast<int>(a) >= static_cast<int>(b);
}

enum class Associativity { NONE, LEFT, RIGHT };

struct ParserRule {
  NonTerminal type;
  std::vector<SymbolType> symbols;
  Precedence precedence;
  Associativity associativity;
  ParserSymbol (*reduce)(std::vector<ParserSymbol> &symbols);
};

static ParserRule
parserRule(NonTerminal type, std::vector<SymbolType> symbols,
           Precedence precedence, Associativity associativity,
           ParserSymbol (*reduce)(std::vector<ParserSymbol> &)) {
  return {type, symbols, precedence, associativity, reduce};
}

template <NonTerminal type, typename NodeClass, int... symbolIndices>
std::unique_ptr<AstNode> simpleReducer(std::vector<ParserSymbol> &symbols) {
  return std::make_unique<NodeClass>(std::move(
      std::get<std::unique_ptr<AstNode>>(symbols[symbolIndices].value))...);
}

static ParserRule parserRules[] = {parserRule(
    NonTerminal::COMPILATION_UNIT, {NonTerminal::DEFINITIONS}, Precedence::NONE,
    Associativity::NONE, [](auto &symbols) {
      return ParserSymbol{NonTerminal::COMPILATION_UNIT,
                          std::make_unique<CompilationUnitNode>(std::move(
                              std::get<std::vector<std::unique_ptr<AstNode>>>(
                                  symbols[0].value)))};
    })};

struct RuleMatch {
  bool canReduce;
  bool shouldShift;
  ParserRule &rule;
  size_t matchingSymbolCount;
};

std::unique_ptr<AstNode> Parser::parse() {
  std::vector<ParserSymbol> stack;
  Token lookahead = lexer.nextToken();
  while (stack.size() < 1 ||
         stack[0].type != SymbolType{NonTerminal::COMPILATION_UNIT}) {
    std::vector<RuleMatch> matches;
    for (auto &rule : parserRules) {
      size_t initialStackIndex = stack.size() > rule.symbols.size()
                                     ? stack.size() - rule.symbols.size()
                                     : 0;
      size_t symbolIndex = 0;
      for (size_t stackIndex = initialStackIndex; stackIndex < stack.size();
           stackIndex++) {
        if (rule.symbols[symbolIndex] != stack[stackIndex].type) {
          symbolIndex = 0;
        } else {
          symbolIndex++;
        }
      }
      size_t matchingSymbolCount = symbolIndex;
      if (matchingSymbolCount == rule.symbols.size()) {
        matches.push_back({true, false, rule, matchingSymbolCount});
      } else if (matchingSymbolCount > 0 || stack.size() == 0) {
        if (rule.symbols[symbolIndex] == SymbolType{lookahead.type}) {
          matches.push_back({false, true, rule, matchingSymbolCount});
        }
      }
    }
    if (matches.size() == 0) {
      throw std::runtime_error("Syntax error: unexpected "s +
                               tokenTypeToString(lookahead.type));
    }
    RuleMatch *dominantMatch = nullptr;
    for (auto &match : matches) {
      if (!dominantMatch ||
          match.matchingSymbolCount > dominantMatch->matchingSymbolCount) {
        dominantMatch = &match;
      }
    }
    if (dominantMatch->canReduce) {
      std::vector<ParserSymbol> symbols;
      for (size_t i = 0; i < dominantMatch->matchingSymbolCount; i++) {
        symbols.push_back(std::move(stack.back()));
        stack.pop_back();
      }
      stack.push_back(dominantMatch->rule.reduce(symbols));
    } else if (dominantMatch->shouldShift) {
      stack.push_back(ParserSymbol{lookahead.type, lookahead.value});
      lookahead = lexer.nextToken();
    }
  }
  return std::move(std::get<std::unique_ptr<AstNode>>(stack[0].value));
}
} // namespace sl
