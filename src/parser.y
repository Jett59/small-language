%require "3.8"
%language "c++"
%define api.token.constructor
%define api.value.type variant
%define api.value.automove
%define parse.lac full

%define api.namespace { sl }

%define api.parser.class  { Parser }

%code requires {
    #include "ast.h"

    namespace sl {
        class Lexer;
    }
}

%debug

%locations

%define parse.error verbose

%{

#include <cstdint>
#include <iostream>
#include <memory>
#include <vector>
#include "error.h"
#include "ast.h"
#include "parser.hh"
#include "lexer.h"

sl::Parser::symbol_type yylex(sl::Lexer& lexer) {
    return lexer.nextToken();
}

using std::make_unique;

template<typename NodeClass, typename... ArgumentTypes>
static std::unique_ptr<NodeClass> makeAstNode(sl::location &location, ArgumentTypes... args) {
    auto result = make_unique<NodeClass>(std::forward<ArgumentTypes>(args)...);
    result->line = location.begin.line;
    result->column = location.begin.column;
    return result;
}

%}

%lex-param { sl::Lexer& lexer }
%parse-param { sl::Lexer& lexer }
%parse-param { std::string fileName }
%parse-param {std::unique_ptr<sl::AstNode> *ast}

%initial-action {
    // Set the file name on the initial location (goes into compilation-unit).
    @$.initialize(&fileName);
}

%token <std::string> IDENTIFIER STRING_LITERAL INTEGER_LITERAL FLOAT_LITERAL

%token LET "let" MUT "mut"
%token FN "fn" RETURN "return"
%token FOR "for" WHILE "while" IF "if" ELSE "else"
%token EXTERN "extern" AS "as"

%token I8 "i8" I16 "i16" I32 "i32" I64 "i64" U8 "u8" U16 "u16" U32 "u32" U64 "u64" F32 "f32" F64 "f64" BOOL "bool" CHAR "char" STRING "string" NIL "nil"
%token TRUE "true" FALSE "false"
%token ARROW "->"
%token LEFT_PAREN "(" RIGHT_PAREN ")" LEFT_BRACE "{" RIGHT_BRACE "}" LEFT_BRACKET "[" RIGHT_BRACKET "]"
%token DOT "." COMMA "," COLON ":" SEMICOLON ";"
%token BANG "!" QUESTION "?"
%token PLUS "+" MINUS "-" STAR "*" SLASH "/" PERCENT "%"
%token AMPERSAND "&" PIPE "|" CARET "^" TILDE "~"
%token AMPERSAND_AMPERSAND "&&" PIPE_PIPE "||"
%token EQUALS "=" EQUALS_EQUALS "==" BANG_EQUALS "!="
%token LESS "<" LESS_EQUALS "<=" GREATER ">" GREATER_EQUALS ">="

%start compilation-unit

%type <std::unique_ptr<AstNode>> statement expression
%type <std::vector<std::unique_ptr<AstNode>>> expression-list statement-list

%%

compilation-unit: statement-list {
    *ast = makeAstNode<CompilationUnitNode>(@1, $1);
}

statement-list: statement {
    std::vector<std::unique_ptr<AstNode>> list;
    list.push_back(std::move($1));
    $$ = std::move(list);
}
| statement-list statement {
    auto list = $1;
    list.push_back($2);
    $$ = std::move(list);
}

statement:
  "let" IDENTIFIER "=" expression ";" {
    $$ = makeAstNode<DefinitionNode>(@1, $2, $4, true);
}
| "mut" IDENTIFIER "=" expression ";" {
    $$ = makeAstNode<DefinitionNode>(@1, $2, $4, false);
}

expression:
    IDENTIFIER {
        $$ = makeAstNode<VariableReferenceNode>(@1, $1);
    }
    | STRING_LITERAL {
        $$ = makeAstNode<StringLiteralNode>(@1, $1);
    }
    | INTEGER_LITERAL {
        $$ = makeAstNode<IntegerLiteralNode>(@1, $1);
    }
    | FLOAT_LITERAL {
        $$ = makeAstNode<FloatLiteralNode>(@1, $1);
    }
    | "true" {
        $$ = makeAstNode<BooleanLiteralNode>(@1, true);
    }
    | "false" {
        $$ = makeAstNode<BooleanLiteralNode>(@1, false);
    }

%%

void sl::Parser::error(const sl::location& location, const std::string& message) {
    throw sl::SlException(location.begin.line, location.begin.column, message);
}
