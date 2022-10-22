#ifndef SL_CODEGEN_H
#define SL_CODEGEN_H

#include "ast.h"

namespace sl {
void codegen(const AstNode &ast, const std::string &targetTriple);
}

#endif