#ifndef SL_CODEGEN_H
#define SL_CODEGEN_H

#include "ast.h"

namespace sl {
enum class GeneratedFileType { OBJECT, ASSEMBLY };

void codegen(const AstNode &ast, const std::string &targetTriple,
             GeneratedFileType fileType, const std::string &outputFile);
} // namespace sl

#endif