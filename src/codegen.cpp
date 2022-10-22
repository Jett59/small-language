#include "codegen.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include <iostream>

namespace sl {
using namespace llvm;

void codegen(const AstNode &ast, const std::string &targetTriple) {
  LLVMContext llvmContext;
  Module module("sl", llvmContext);
  if (targetTriple != "") {
    std::string err;
    InitializeAllTargets();
    auto target = TargetRegistry::lookupTarget(targetTriple, err);
    if (!target) {
      std::cerr << "error: " << err << std::endl;
      return;
    }
    // Create the target machine.
    auto cpu = "generic";
    auto features = "";
    TargetOptions opt;
    auto rm = Optional<Reloc::Model>();
    auto targetMachine =
        target->createTargetMachine(targetTriple, cpu, features, opt, rm);
    module.setTargetTriple(targetTriple);
    module.setDataLayout(targetMachine->createDataLayout());
  } else {
    module.setTargetTriple(sys::getDefaultTargetTriple());
  }
  module.print(errs(), nullptr);
}
} // namespace sl
