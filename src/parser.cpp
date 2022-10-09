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
  int line, column;
};

enum class Precedence {
  NONE,
  // For things like the STATEMENT_LIST which require precedence to specify that
  // the list initializer should not be used if appending is an option.
  LIST_INITIALIZE,
  LIST_APPEND,
  // Operators
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
  // Quite ugly code but it is hard with parameter packs.
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

template <NonTerminal type, typename ElementClass, int listIndex,
          int additionalIndex>
ParserSymbol listReducer(std::vector<ParserSymbol> &symbols) {
  if (listIndex >= 0) {
    auto list = std::move(std::get<std::vector<std::unique_ptr<ElementClass>>>(
        symbols[listIndex].value));
    auto additional = std::move(std::get<std::unique_ptr<ElementClass>>(
        symbols[additionalIndex].value));
    list.push_back(std::move(additional));
    return {type, std::move(list)};
  } else {
    auto list = std::vector<std::unique_ptr<ElementClass>>();
    auto additional = std::move(std::get<std::unique_ptr<ElementClass>>(
        symbols[additionalIndex].value));
    list.push_back(std::move(additional));
    return {type, std::move(list)};
  }
}

static ParserRule parserRules[] = {
    parserRule(
        NonTerminal::COMPILATION_UNIT,
        {NonTerminal::STATEMENT_LIST, TokenType::END}, Precedence::NONE,
        Associativity::NONE,
        simpleReducer<NonTerminal::COMPILATION_UNIT, CompilationUnitNode,
                      IndexAndType<0, std::vector<std::unique_ptr<AstNode>>>>),
    parserRule(NonTerminal::STATEMENT_LIST, {NonTerminal::STATEMENT},
               Precedence::LIST_INITIALIZE, Associativity::NONE,
               listReducer<NonTerminal::STATEMENT_LIST, AstNode, -1, 0>),
    parserRule(NonTerminal::STATEMENT_LIST,
               {NonTerminal::STATEMENT_LIST, NonTerminal::STATEMENT},
               Precedence::LIST_APPEND, Associativity::NONE,
               listReducer<NonTerminal::STATEMENT_LIST, AstNode, 0, 1>),
    parserRule(NonTerminal::STATEMENT,
               {TokenType::LET, TokenType::IDENTIFIER, TokenType::EQUALS,
                NonTerminal::EXPRESSION, TokenType::SEMICOLON},
               Precedence::NONE, Associativity::NONE,
               simpleReducer<NonTerminal::STATEMENT, DefinitionNode,
                             IndexAndType<1, std::string>, IndexAndType<3>,
                             IndexAndType<0, std::string>>),
    parserRule(NonTerminal::STATEMENT,
               {TokenType::MUT, TokenType::IDENTIFIER, TokenType::EQUALS,
                NonTerminal::EXPRESSION, TokenType::SEMICOLON},
               Precedence::NONE, Associativity::NONE,
               simpleReducer<NonTerminal::STATEMENT, DefinitionNode,
                             IndexAndType<1, std::string>, IndexAndType<3>,
                             IndexAndType<0, std::string>>),
    parserRule(NonTerminal::EXPRESSION, {TokenType::INTEGER}, Precedence::NONE,
               Associativity::NONE,
               simpleReducer<NonTerminal::EXPRESSION, IntegerLiteralNode,
                             IndexAndType<0, std::string>>),
    parserRule(NonTerminal::EXPRESSION, {TokenType::FLOAT}, Precedence::NONE,
               Associativity::NONE,
               simpleReducer<NonTerminal::EXPRESSION, FloatLiteralNode,
                             IndexAndType<0, std::string>>),
    parserRule(NonTerminal::EXPRESSION, {TokenType::STRING}, Precedence::NONE,
               Associativity::NONE,
               simpleReducer<NonTerminal::EXPRESSION, StringLiteralNode,
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
  while (stack.size() != 1 ||
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
      if (!dominantMatch || dominantMatch->matchingSymbolCount == 0 ||
          match.rule.precedence > dominantMatch->rule.precedence) {
        dominantMatch = &match;
      } else if (match.matchingSymbolCount > 0 && dominantMatch &&
                 dominantMatch->rule.precedence == match.rule.precedence) {
        if (match.canReduce && dominantMatch->canReduce) {
          std::cout << "Reduce/reduce conflict: " << std::endl;
          printStack(stack);
          std::cout << "Reduction 1: ";
          for (auto &symbol : match.rule.symbols) {
            std::cout << " ";
            if (std::holds_alternative<TokenType>(symbol)) {
              std::cout << tokenTypeToString(std::get<TokenType>(symbol));
            } else {
              std::cout << nonTerminalToString(std::get<NonTerminal>(symbol));
            }
          }
          std::cout << " -> " << nonTerminalToString(match.rule.type)
                    << std::endl;
          std::cout << "Reduction 2: ";
          for (auto &symbol : dominantMatch->rule.symbols) {
            std::cout << " ";
            if (std::holds_alternative<TokenType>(symbol)) {
              std::cout << tokenTypeToString(std::get<TokenType>(symbol));
            } else {
              std::cout << nonTerminalToString(std::get<NonTerminal>(symbol));
            }
          }
          std::cout << " -> " << nonTerminalToString(dominantMatch->rule.type)
                    << std::endl;
          throw std::logic_error("Ambiguous grammar");
        } else if (dominantMatch->canReduce != match.canReduce) {
          std::cout << "Shift/reduce conflict: " << std::endl;
          printStack(stack);
          auto &reducingRule =
              dominantMatch->canReduce ? dominantMatch->rule : match.rule;
          std::cout << "Reduction: ";
          for (auto &symbol : reducingRule.symbols) {
            std::cout << " ";
            if (std::holds_alternative<TokenType>(symbol)) {
              std::cout << tokenTypeToString(std::get<TokenType>(symbol));
            } else {
              std::cout << nonTerminalToString(std::get<NonTerminal>(symbol));
            }
          }
          std::cout << " -> " << nonTerminalToString(dominantMatch->rule.type)
                    << std::endl;
          std::cout << "Shift: ";
          std::cout << tokenTypeToString(lookahead.type);
          std::cout << std::endl;
          std::cout << "  To match this rule:" << std::endl;
          auto &shiftingRule =
              dominantMatch->canReduce ? match.rule : dominantMatch->rule;
          for (auto &symbol : shiftingRule.symbols) {
            std::cout << " ";
            if (std::holds_alternative<TokenType>(symbol)) {
              std::cout << tokenTypeToString(std::get<TokenType>(symbol));
            } else {
              std::cout << nonTerminalToString(std::get<NonTerminal>(symbol));
            }
          }
          std::cout << std::endl;
          throw std::logic_error("Ambiguous grammar");
        }
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
