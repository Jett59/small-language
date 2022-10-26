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
%type <std::unique_ptr<Type>> type
%type <std::vector<std::unique_ptr<Type>>> type-list
%type <std::unique_ptr<NameAndType>> name-and-type
%type <std::vector<std::unique_ptr<NameAndType>>> name-and-type-list

%left "+" "-"
%left "*" "/" "%"
%left "&" "|" "^" "~"
%left "==" "!=" "<" "<=" ">" ">="
%left "&&" "||"
%right "="

/* For function calls */
%left "("

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
| "if" expression "{" statement-list "}" {
    $$ = makeAstNode<IfStatementNode>(@1, $2, $4);
}
| "if" expression "{" statement-list "}" "else" "{" statement-list "}" {
    $$ = makeAstNode<IfStatementNode>(@1, $2, $4, $8);
}
| "return" expression ";" {
    $$ = makeAstNode<ReturnNode>(@1, $2);
}
| "return" ";" {
    $$ = makeAstNode<ReturnNode>(@1);
}
| expression ";" {
    $$ = $1;
}

expression-list: expression {
    std::vector<std::unique_ptr<AstNode>> list;
    list.push_back($1);
    $$ = std::move(list);
}
| expression-list "," expression {
    auto list = $1;
    list.push_back($3);
    $$ = std::move(list);
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
| "fn" "(" name-and-type-list ")" "->" type "{" statement-list "}" {
    $$ = makeAstNode<FunctionNode>(@1, $6, $3, $8);
}
| "extern" name-and-type {
    $$ = makeAstNode<ExternalNode>(@1, $2);
}
| expression "(" expression-list ")" {
    $$ = makeAstNode<CallNode>(@1, $1, $3);
}
| expression "+" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::ADD, $1, $3);
}
| expression "-" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::SUBTRACT, $1, $3);
}
| expression "*" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::MULTIPLY, $1, $3);
}
| expression "/" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::DIVIDE, $1, $3);
}
| expression "%" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::MODULO, $1, $3);
}
| expression "==" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::EQUAL, $1, $3);
}
| expression "!=" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::NOT_EQUAL, $1, $3);
}
| expression "<" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::LESS_THAN, $1, $3);
}
| expression "<=" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::LESS_THAN_OR_EQUAL, $1, $3);
}
| expression ">" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::GREATER_THAN, $1, $3);
}
| expression ">=" expression {
    $$ = makeAstNode<BinaryOperatorNode>(@1, BinaryOperatorType::GREATER_THAN_OR_EQUAL, $1, $3);
}

name-and-type-list: name-and-type {
    std::vector<std::unique_ptr<NameAndType>> list;
    list.push_back(std::move($1));
    $$ = std::move(list);
}
| name-and-type-list "," name-and-type {
    auto list = $1;
    list.push_back($3);
    $$ = std::move(list);
}

name-and-type: IDENTIFIER ":" type {
    $$ = make_unique<NameAndType>($1, $3);
}

type-list: type {
    std::vector<std::unique_ptr<Type>> list;
    list.push_back(std::move($1));
    $$ = std::move(list);
}
| type-list "," type {
    auto list = $1;
    list.push_back($3);
    $$ = std::move(list);
}

type:
    "i8" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::I8);
    }
    | "i16" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::I16);
    }
        | "i32" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::I32);
    }
    | "i64" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::I64);
    }
    | "u8" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::U8);
    }
    | "u16" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::U16);
    }
    | "u32" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::U32);
    }
    | "u64" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::U64);
    }
    | "f32" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::F32);
    }
    | "f64" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::F64);
    }
    | "bool" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::BOOL);
    }
    | "char" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::CHAR);
    }
    | "string" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::STRING);
    }
    | "nil" {
        $$ = make_unique<PrimitiveTypeNode>( PrimitiveType::NIL);
    }
    | "(" type-list ")" "->" type {
        $$ = make_unique<FunctionTypeNode>($2, $5);
    }

%%

void sl::Parser::error(const sl::location& location, const std::string& message) {
    throw sl::SlException(location.begin.line, location.begin.column, message);
}
