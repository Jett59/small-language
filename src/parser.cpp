#include "parser.h"
#include <iostream>
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

template <int i, typename T = std::unique_ptr<AstNode>> struct IndexAndType {
  static constexpr int index = i;
  using Type = T;
};

void assertThat(bool condition, const std::string &message) {
  if (!condition) {
    std::cerr << message << std::endl;
    std::exit(1);
  }
}

template <NonTerminal type, typename NodeClass, typename... symbolIndices>
ParserSymbol simpleReducer(std::vector<ParserSymbol> &symbols) {
  // Assert that the symbols have the expected types.
  (void)std::initializer_list<int>{
      (assertThat(
           std::holds_alternative<typename symbolIndices::Type>(
               symbols[symbolIndices::index].value),
           "Expected symbol "s + std::to_string(symbolIndices::index) +
               " not to be "s +
               std::to_string(symbols[symbolIndices::index].value.index())),
       0)...};
  auto value = std::make_unique<NodeClass>(
      std::move(std::get<typename symbolIndices::Type>(
          symbols[symbolIndices::index].value))...);
  return {type, std::move(value)};
}

static ParserRule parserRules[] = {
    parserRule(
        NonTerminal::COMPILATION_UNIT,
        {NonTerminal::DEFINITIONS, TokenType::END}, Precedence::NONE,
        Associativity::NONE,
        simpleReducer<NonTerminal::COMPILATION_UNIT, CompilationUnitNode,
                      IndexAndType<0, std::vector<std::unique_ptr<AstNode>>>>),
    parserRule(NonTerminal::DEFINITION,
               {TokenType::LET, TokenType::IDENTIFIER, TokenType::EQUALS,
                NonTerminal::EXPRESSION, TokenType::SEMICOLON},
               Precedence::NONE, Associativity::NONE,
               simpleReducer<NonTerminal::DEFINITION, VariableDefinitionNode,
                             IndexAndType<1, std::string>, IndexAndType<3>>),
    parserRule(NonTerminal::EXPRESSION, {TokenType::INTEGER}, Precedence::NONE,
               Associativity::NONE,
               simpleReducer<NonTerminal::EXPRESSION, IntegerLiteralNode,
                             IndexAndType<0, std::string>>)};

struct RuleMatch {
  bool canReduce;
  bool shouldShift;
  ParserRule &rule;
  size_t matchingSymbolCount;
};

static void printStack(std::vector<ParserSymbol> &stack) {
  for (auto &symbol : stack) {
    std::cout << " ";
    if (std::holds_alternative<TokenType>(symbol.type)) {
      std::cout << tokenTypeToString(std::get<TokenType>(symbol.type));
    } else {
      std::cout << nonTerminalToString(std::get<NonTerminal>(symbol.type));
    }
  }
  std::cout << std::endl;
}

std::unique_ptr<AstNode> Parser::parse() {
  std::vector<ParserSymbol> stack;
  Token lookahead = lexer.nextToken();
  while (stack.size() < 1 ||
         stack[0].type != SymbolType{NonTerminal::COMPILATION_UNIT}) {
    if (lookahead.type == TokenType::ERROR) {
      throw std::runtime_error("Error while parsing: "s + lookahead.value);
    }
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
      } else if (rule.symbols[symbolIndex] == SymbolType{lookahead.type}) {
        matches.push_back({false, true, rule, matchingSymbolCount});
      }
    }
    if (matches.size() == 0) {
      throw std::runtime_error("Syntax error: unexpected "s +
                               tokenTypeToString(lookahead.type));
    }
    RuleMatch *dominantMatch = nullptr;
    for (auto &match : matches) {
      if (!dominantMatch ||
          (match.matchingSymbolCount > dominantMatch->matchingSymbolCount &&
           match.rule.precedence >= dominantMatch->rule.precedence) ||
          match.rule.precedence > dominantMatch->rule.precedence) {
        dominantMatch = &match;
      }
    }
    if (dominantMatch->canReduce) {
      std::vector<ParserSymbol> symbols;
      for (size_t i = 0; i < dominantMatch->matchingSymbolCount; i++) {
        symbols.push_back(std::move(stack.back()));
        stack.pop_back();
      }
      std::reverse(symbols.begin(), symbols.end());
      stack.push_back(dominantMatch->rule.reduce(symbols));
    } else if (dominantMatch->shouldShift) {
      stack.push_back(ParserSymbol{lookahead.type, lookahead.value});
      lookahead = lexer.nextToken();
    }
    printStack(stack);
  }
  return std::move(std::get<std::unique_ptr<AstNode>>(stack[0].value));
}
} // namespace sl
