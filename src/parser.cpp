#include "parser.h"
#include "error.h"
#include <iostream>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

namespace sl {
using std::string_literals::operator""s;

using SymbolType = std::variant<TokenType, NonTerminal>;

using AstNodePointer = std::unique_ptr<AstNode>;
using AstNodeList = std::vector<AstNodePointer>;

struct ParserSymbol {
  SymbolType type;
  std::variant<std::nullptr_t, AstNodePointer, AstNodeList, std::string,
               std::unique_ptr<Type>, std::unique_ptr<NameAndType>,
               std::vector<std::unique_ptr<NameAndType>>>
      value;
  int line, column;
};

enum class Precedence {
  LOWEST,
  DEFAULT,
  DEFINITION, // To avoid conflicts with expression statements (E.G. is
  // let x = 7;
  //  Parsed as let x = STATEMENT or simply STATEMENT)
  //  For things like the STATEMENT_LIST which require precedence to specify
  //  that the list initializer should not be used if appending is an option.
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
  CAST,
};

static inline std::string symbolTypeToString(const SymbolType &type) {
  if (std::holds_alternative<TokenType>(type)) {
    return tokenTypeToString(std::get<TokenType>(type));
  } else {
    return nonTerminalToString(std::get<NonTerminal>(type));
  }
}

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

enum class Associativity { DEFAULT, LEFT, RIGHT };

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

template <int i, typename T = AstNodePointer> struct IndexAndType {
  static constexpr int index = i;
  using Type = T;
};

void assertThat(bool condition, const std::string &message) {
  if (!condition) {
    std::cerr << message << std::endl;
    std::exit(1);
  }
}

template <NonTerminal type, typename NodeClass, int locationSymbolIndex,
          typename... symbolIndices>
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
  int line = symbols[locationSymbolIndex].line;
  int column = symbols[locationSymbolIndex].column;
  if constexpr (std::is_base_of<AstNode, NodeClass>()) {
    value->line = line;
    value->column = column;
  }
  return {type, std::move(value), line, column};
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

template <PrimitiveType type>
static ParserRule primitiveTypeRule(SymbolType symbol) {
  return parserRule(NonTerminal::TYPE, {symbol}, Precedence::DEFAULT,
                    Associativity::DEFAULT,
                    [](std::vector<ParserSymbol> &symbols) {
                      return ParserSymbol{
                          NonTerminal::TYPE,
                          std::make_unique<PrimitiveTypeNode>(type),
                      };
                    });
}

template <BinaryOperatorType type>
static ParserRule
binaryOperatorRule(SymbolType symbol, Precedence precedence,
                   Associativity associativity = Associativity::LEFT) {
  return parserRule(
      NonTerminal::EXPRESSION,
      {NonTerminal::EXPRESSION, symbol, NonTerminal::EXPRESSION}, precedence,
      associativity, [](std::vector<ParserSymbol> &symbols) {
        auto value = std::make_unique<BinaryOperatorNode>(
            type, std::get<AstNodePointer>(std::move(symbols[0].value)),
            std::get<AstNodePointer>(std::move(symbols[2].value)));
        int line = symbols[1].line;
        int column = symbols[1].column;
        value->line = line;
        value->column = column;
        return ParserSymbol{
            NonTerminal::EXPRESSION,
            std::move(value),
            line,
            column,
        };
      });
}

template <size_t index, NonTerminal type>
static ParserRule
restructureParserRule(const std::vector<SymbolType> &symbols,
                      Precedence precedence = Precedence::DEFAULT) {
  return ParserRule{type, symbols, precedence, Associativity::DEFAULT,
                    [](std::vector<ParserSymbol> &symbols) {
                      auto symbol = std::move(symbols[index]);
                      symbol.type = type;
                      return std::move(symbol);
                    }};
}

template <NonTerminal type, typename NodeClass, int locationSymbolIndex,
          typename... symbolIndices>
ParserRule simpleRule(std::vector<SymbolType> symbols,
                      Precedence precedence = Precedence::DEFAULT,
                      Associativity associativity = Associativity::DEFAULT) {
  return parserRule(
      type, symbols, precedence, associativity,
      simpleReducer<type, NodeClass, locationSymbolIndex, symbolIndices...>);
}

static ParserRule parserRules[] = {
    simpleRule<NonTerminal::COMPILATION_UNIT, CompilationUnitNode, 0,
               IndexAndType<0, AstNodeList>>(
        {NonTerminal::STATEMENT_LIST, TokenType::END}, Precedence::DEFAULT,
        Associativity::DEFAULT),
    parserRule(NonTerminal::STATEMENT_LIST, {NonTerminal::STATEMENT},
               Precedence::LIST_INITIALIZE, Associativity::DEFAULT,
               listReducer<NonTerminal::STATEMENT_LIST, AstNode, -1, 0>),
    parserRule(NonTerminal::STATEMENT_LIST,
               {NonTerminal::STATEMENT_LIST, NonTerminal::STATEMENT},
               Precedence::LIST_APPEND, Associativity::DEFAULT,
               listReducer<NonTerminal::STATEMENT_LIST, AstNode, 0, 1>),
    simpleRule<NonTerminal::STATEMENT, DefinitionNode, 2,
               IndexAndType<1, std::string>, IndexAndType<3>,
               IndexAndType<0, std::string>>(
        {TokenType::LET, TokenType::IDENTIFIER, TokenType::EQUALS,
         NonTerminal::EXPRESSION, TokenType::SEMICOLON},
        Precedence::DEFINITION, Associativity::DEFAULT),
    simpleRule<NonTerminal::STATEMENT, DefinitionNode, 2,
               IndexAndType<1, std::string>, IndexAndType<3>,
               IndexAndType<0, std::string>>(
        {TokenType::MUT, TokenType::IDENTIFIER, TokenType::EQUALS,
         NonTerminal::EXPRESSION, TokenType::SEMICOLON},
        Precedence::DEFINITION, Associativity::DEFAULT),
    restructureParserRule<0, NonTerminal::STATEMENT>(
        {NonTerminal::EXPRESSION, TokenType::SEMICOLON}, Precedence::DEFAULT),
    restructureParserRule<1, NonTerminal::EXPRESSION>({TokenType::LEFT_PAREN,
                                                       NonTerminal::EXPRESSION,
                                                       TokenType::RIGHT_PAREN}),
    simpleRule<NonTerminal::EXPRESSION, IntegerLiteralNode, 0,
               IndexAndType<0, std::string>>(
        {TokenType::INTEGER}, Precedence::DEFAULT, Associativity::DEFAULT),
    simpleRule<NonTerminal::EXPRESSION, FloatLiteralNode, 0,
               IndexAndType<0, std::string>>(
        {TokenType::FLOAT}, Precedence::DEFAULT, Associativity::DEFAULT),
    simpleRule<NonTerminal::EXPRESSION, StringLiteralNode, 0,
               IndexAndType<0, std::string>>(
        {TokenType::STRING}, Precedence::DEFAULT, Associativity::DEFAULT),
    simpleRule<NonTerminal::EXPRESSION, BoolLiteralNode, 0,
               IndexAndType<0, std::string>>({TokenType::BOOL_LITERAL}),
    primitiveTypeRule<PrimitiveType::NIL>(TokenType::NIL),
    primitiveTypeRule<PrimitiveType::I8>(TokenType::I8),
    primitiveTypeRule<PrimitiveType::I16>(TokenType::I16),
    primitiveTypeRule<PrimitiveType::I32>(TokenType::I32),
    primitiveTypeRule<PrimitiveType::I64>(TokenType::I64),
    primitiveTypeRule<PrimitiveType::U8>(TokenType::U8),
    primitiveTypeRule<PrimitiveType::U16>(TokenType::U16),
    primitiveTypeRule<PrimitiveType::U32>(TokenType::U32),
    primitiveTypeRule<PrimitiveType::U64>(TokenType::U64),
    primitiveTypeRule<PrimitiveType::F32>(TokenType::F32),
    primitiveTypeRule<PrimitiveType::F64>(TokenType::F64),
    primitiveTypeRule<PrimitiveType::BOOL>(TokenType::BOOL),
    primitiveTypeRule<PrimitiveType::CHAR>(TokenType::CHAR),
    primitiveTypeRule<PrimitiveType::STRING>(TokenType::STRING_TYPE),
    simpleRule<NonTerminal::NAME_AND_TYPE, NameAndType, 0,
               IndexAndType<0, std::string>,
               IndexAndType<2, std::unique_ptr<Type>>>(
        {TokenType::IDENTIFIER, TokenType::COLON, NonTerminal::TYPE},
        Precedence::DEFAULT, Associativity::DEFAULT),
    parserRule(
        NonTerminal::NAME_AND_TYPE_LIST, {NonTerminal::NAME_AND_TYPE},
        Precedence::LOWEST, Associativity::DEFAULT,
        listReducer<NonTerminal::NAME_AND_TYPE_LIST, NameAndType, -1, 0>),
    parserRule(NonTerminal::NAME_AND_TYPE_LIST,
               {NonTerminal::NAME_AND_TYPE_LIST, TokenType::COMMA,
                NonTerminal::NAME_AND_TYPE},
               Precedence::DEFAULT, Associativity::DEFAULT,
               listReducer<NonTerminal::NAME_AND_TYPE_LIST, NameAndType, 0, 2>),
    simpleRule<NonTerminal::EXPRESSION, FunctionNode, 0,
               IndexAndType<5, std::unique_ptr<Type>>,
               IndexAndType<2, std::vector<std::unique_ptr<NameAndType>>>,
               IndexAndType<7, AstNodeList>>(
        {TokenType::FN, TokenType::LEFT_PAREN, NonTerminal::NAME_AND_TYPE_LIST,
         TokenType::RIGHT_PAREN, TokenType::ARROW, NonTerminal::TYPE,
         TokenType::LEFT_BRACE, NonTerminal::STATEMENT_LIST,
         TokenType::RIGHT_BRACE},
        Precedence::DEFAULT, Associativity::DEFAULT),
    simpleRule<NonTerminal::EXPRESSION, CastNode, 1, IndexAndType<0>,
               IndexAndType<2, std::unique_ptr<Type>>>(
        {NonTerminal::EXPRESSION, TokenType::AS, NonTerminal::TYPE},
        Precedence::CAST, Associativity::LEFT),
    binaryOperatorRule<BinaryOperatorType::ADD>(TokenType::PLUS,
                                                Precedence::SUM),
    binaryOperatorRule<BinaryOperatorType::SUBTRACT>(TokenType::MINUS,
                                                     Precedence::SUM),
    binaryOperatorRule<BinaryOperatorType::MULTIPLY>(TokenType::STAR,
                                                     Precedence::PRODUCT),
    binaryOperatorRule<BinaryOperatorType::DIVIDE>(TokenType::SLASH,
                                                   Precedence::PRODUCT),
    binaryOperatorRule<BinaryOperatorType::MODULO>(TokenType::PERCENT,
                                                   Precedence::PRODUCT),
    binaryOperatorRule<BinaryOperatorType::EQUAL>(TokenType::EQUALS_EQUALS,
                                                  Precedence::COMPARISON),
    binaryOperatorRule<BinaryOperatorType::NOT_EQUAL>(TokenType::BANG_EQUALS,
                                                      Precedence::COMPARISON),
    binaryOperatorRule<BinaryOperatorType::LESS_THAN>(TokenType::LESS,
                                                      Precedence::COMPARISON),
    binaryOperatorRule<BinaryOperatorType::LESS_THAN_OR_EQUAL>(
        TokenType::LESS_EQUALS, Precedence::COMPARISON),
    binaryOperatorRule<BinaryOperatorType::GREATER_THAN>(
        TokenType::GREATER, Precedence::COMPARISON),
    binaryOperatorRule<BinaryOperatorType::GREATER_THAN_OR_EQUAL>(
        TokenType::GREATER_EQUALS, Precedence::COMPARISON),
    binaryOperatorRule<BinaryOperatorType::BITWISE_AND>(TokenType::AMPERSAND,
                                                        Precedence::BITWISE),
    binaryOperatorRule<BinaryOperatorType::BITWISE_OR>(TokenType::PIPE,
                                                       Precedence::BITWISE),
    binaryOperatorRule<BinaryOperatorType::BITWISE_XOR>(TokenType::CARET,
                                                        Precedence::BITWISE),
    binaryOperatorRule<BinaryOperatorType::ASSIGN>(
        TokenType::EQUALS, Precedence::ASSIGNMENT, Associativity::RIGHT),
    simpleRule<NonTerminal::EXPRESSION, NilNode, 0>(
        {TokenType::NIL}, Precedence::DEFAULT, Associativity::DEFAULT),
    simpleRule<NonTerminal::STATEMENT, IfStatementNode, 0, IndexAndType<1>,
               IndexAndType<3, AstNodeList>>(
        {TokenType::IF, NonTerminal::EXPRESSION, TokenType::LEFT_BRACE,
         NonTerminal::STATEMENT_LIST, TokenType::RIGHT_BRACE},
        Precedence::DEFAULT, Associativity::RIGHT),
    simpleRule<NonTerminal::STATEMENT, IfStatementNode, 0, IndexAndType<1>,
               IndexAndType<3, AstNodeList>, IndexAndType<7, AstNodeList>>(
        {TokenType::IF, NonTerminal::EXPRESSION, TokenType::LEFT_BRACE,
         NonTerminal::STATEMENT_LIST, TokenType::RIGHT_BRACE, TokenType::ELSE,
         TokenType::LEFT_BRACE, NonTerminal::STATEMENT_LIST,
         TokenType::RIGHT_BRACE},
        Precedence::DEFAULT, Associativity::RIGHT),
    simpleRule<NonTerminal::EXPRESSION, VariableReferenceNode, 0,
               IndexAndType<0, std::string>>({TokenType::IDENTIFIER})};

struct RuleMatch {
  bool canReduce;
  bool shouldShift;
  const ParserRule &rule;
  size_t matchingSymbolCount;
};

static void printStack(std::vector<ParserSymbol> &stack) {
  for (auto &symbol : stack) {
    std::cout << " " << symbol.line << ":" << symbol.column << " ";
    std::cout << symbolTypeToString(symbol.type);
  }
  std::cout << std::endl;
}

static void printRule(const ParserRule &rule) {
  for (auto &symbol : rule.symbols) {
    std::cout << " ";
    std::cout << symbolTypeToString(symbol);
  }
  std::cout << " -> " << nonTerminalToString(rule.type) << std::endl;
}

static size_t getMatchingSymbolCount(const std::vector<ParserSymbol> &stack,
                                     const ParserRule &rule,
                                     size_t skipInitial = 0) {
  size_t initialStackIndex =
      (stack.size() > rule.symbols.size() ? stack.size() - rule.symbols.size()
                                          : 0) +
      skipInitial;
  if (initialStackIndex >= stack.size()) {
    return 0;
  }
  size_t symbolIndex = 0;
  for (size_t stackIndex = initialStackIndex; stackIndex < stack.size();
       stackIndex++) {
    if (rule.symbols[symbolIndex] != stack[stackIndex].type) {
      symbolIndex = 0;
    } else {
      symbolIndex++;
    }
  }
  return symbolIndex;
}

static std::optional<RuleMatch>
findSymbolMatch(const std::vector<ParserSymbol> &stack, const ParserRule &rule,
                const Token &lookahead) {
  size_t matchingSymbolCount = getMatchingSymbolCount(stack, rule);
  if (matchingSymbolCount == rule.symbols.size()) {
    if (rule.associativity != Associativity::RIGHT) {
      return RuleMatch{true, false, rule, matchingSymbolCount};
    } else {
      size_t nextMatchingSymbolCount = getMatchingSymbolCount(stack, rule, 1);
      assertThat(nextMatchingSymbolCount != matchingSymbolCount,
                 "Symbol counts are the same???");
      if (rule.symbols[nextMatchingSymbolCount] == SymbolType{lookahead.type}) {
        return RuleMatch{false, true, rule, matchingSymbolCount};
      } else {
        return RuleMatch{true, false, rule, matchingSymbolCount};
      }
    }
  } else if (rule.symbols[matchingSymbolCount] == SymbolType{lookahead.type}) {
    return RuleMatch{false, true, rule, matchingSymbolCount};
  }
  return std::nullopt;
}

// Creates a stack with the same symbol types as the given stack but all
// semantic values are removed.
static std::vector<ParserSymbol>
createFakeStack(const std::vector<ParserSymbol> &stack) {
  std::vector<ParserSymbol> fakeStack;
  fakeStack.reserve(stack.size());
  for (const auto &symbol : stack) {
    fakeStack.push_back(ParserSymbol{symbol.type});
  }
  return fakeStack;
}

static bool
canContinueParsingAfterReduction(const std::vector<ParserSymbol> &stack,
                                 const RuleMatch &match,
                                 const Token &lookahead) {
  // Pretend to do the reduction and see if there is a rule to match.
  // If there isn't, don't use this match as dominant.
  std::vector<ParserSymbol> testStack = createFakeStack(stack);
  testStack.resize(testStack.size() - match.matchingSymbolCount);
  testStack.push_back({match.rule.type});
  for (const auto &rule : parserRules) {
    auto match = findSymbolMatch(testStack, rule, lookahead);
    if (match && match->matchingSymbolCount > 0) {
      return true;
    }
  }
  return false;
}

AstNodePointer Parser::parse() {
  std::vector<ParserSymbol> stack;
  Token lookahead = lexer.nextToken();
  SymbolType lastSymbolType = lookahead.type; // Lookahead token type for shift,
                                              // non-terminal for reduce
  while (stack.size() != 1 ||
         stack[0].type != SymbolType{NonTerminal::COMPILATION_UNIT}) {
    if (lookahead.type == TokenType::ERROR) {
      throw std::runtime_error("Error while parsing: "s + lookahead.value);
    }
    std::vector<RuleMatch> matches;
    for (auto &rule : parserRules) {
      auto match = findSymbolMatch(stack, rule, lookahead);
      if (match) {
        matches.push_back(*match);
      }
    }
    RuleMatch *dominantMatch = nullptr;
    for (auto &match : matches) {
      if (!dominantMatch || dominantMatch->matchingSymbolCount == 0 ||
          match.rule.precedence > dominantMatch->rule.precedence) {
        if (match.canReduce &&
            match.rule.type != NonTerminal::COMPILATION_UNIT) {
          dominantMatch = &match;
        } else {
          dominantMatch = &match;
        }
      } else if (match.matchingSymbolCount > 0 && dominantMatch &&
                 dominantMatch->rule.precedence == match.rule.precedence) {
        if (match.canReduce && dominantMatch->canReduce) {
          if (!canContinueParsingAfterReduction(stack, *dominantMatch,
                                                lookahead)) {
            dominantMatch = &match;
            continue;
          }
          bool isValidReduction =
              canContinueParsingAfterReduction(stack, match, lookahead);
          if (isValidReduction) {
            std::cout << "Reduce/reduce conflict: " << std::endl;
            printStack(stack);
            std::cout << "Reduction 1: ";
            printRule(dominantMatch->rule);
            std::cout << "Reduction 2: ";
            printRule(match.rule);
            throw std::logic_error("Ambiguous grammar");
          }
        } else if (dominantMatch->canReduce != match.canReduce) {
          if (dominantMatch->rule.associativity == match.rule.associativity) {
            if (match.rule.associativity == Associativity::LEFT) {
              dominantMatch = match.canReduce ? &match : dominantMatch;
              continue;
            } else if (match.rule.associativity == Associativity::RIGHT) {
              dominantMatch = match.shouldShift ? &match : dominantMatch;
              continue;
            }
          }
          if (dominantMatch->canReduce &&
              !canContinueParsingAfterReduction(stack, *dominantMatch,
                                                lookahead)) {
            dominantMatch = &match;
          } else if (!(match.canReduce && !canContinueParsingAfterReduction(
                                              stack, match, lookahead))) {
            std::cout << "Shift/reduce conflict: " << std::endl;
            printStack(stack);
            auto &reducingRule =
                dominantMatch->canReduce ? dominantMatch->rule : match.rule;
            std::cout << "Reduction: ";
            printRule(reducingRule);
            std::cout << "Shift: ";
            std::cout << tokenTypeToString(lookahead.type) << std::endl;
            std::cout << "  To match this rule:" << std::endl;
            auto &shiftingRule =
                dominantMatch->canReduce ? match.rule : dominantMatch->rule;
            std::cout << "  ";
            printRule(shiftingRule);
            throw std::logic_error("Ambiguous grammar");
          }
        }
      }
    }
    if (!dominantMatch) {
      throw SlException(lookahead.line, lookahead.column,
                        "Syntax error: unexpected "s +
                            symbolTypeToString(lastSymbolType));
    }
    if (dominantMatch->canReduce) {
      lastSymbolType = dominantMatch->rule.type;
      std::vector<ParserSymbol> symbols;
      for (size_t i = 0; i < dominantMatch->matchingSymbolCount; i++) {
        symbols.push_back(std::move(stack.back()));
        stack.pop_back();
      }
      std::reverse(symbols.begin(), symbols.end());
      stack.push_back(dominantMatch->rule.reduce(symbols));
    } else if (dominantMatch->shouldShift) {
      stack.push_back(ParserSymbol{lookahead.type, lookahead.value,
                                   lookahead.line, lookahead.column});
      lookahead = lexer.nextToken();
      lastSymbolType = lookahead.type;
    }
    // printStack(stack);
  }
  return std::move(std::get<AstNodePointer>(stack[0].value));
}
} // namespace sl
