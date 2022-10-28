#include "codegen.h"
#include "error.h"
#include "lexer.h"
#include "parser.hh"
#include <fstream>
#include <iostream>

using namespace sl;

static void usage(char *program) {
  std::cout << "Usage: " << program << " [options] file" << std::endl;
}

static void help(char *program) {
  usage(program);
  std::cout << "Options:" << std::endl;
  std::cout << "-t, --target <triple>\t\tSpecify the LLVm target triple for "
               "cross-compilation"
            << std::endl;
  std::cout << "-c\t\t\tCompile the source file to an object file" << std::endl;
  std::cout << "-S\t\t\tCompile the source file to an assembly file"
            << std::endl;
  std::cout << "--print-ir\t\tPrint the LLVM IR to stdout" << std::endl;
  std::cout << "  -h, --help\t\tShow this help message" << std::endl;
  std::cout << "  -v, --version\t\tShow version information" << std::endl;
}

#define MAJOR_VERSION 0
#define MINOR_VERSION 0
#define PATCH_VERSION 1

struct Options {
  bool help;
  bool version;
  std::string file;
  std::string target;
  GeneratedFileType outputFileType = GeneratedFileType::OBJECT;
  std::string outputFile;
  bool printIr = false;

  Options(int argc, char **argv) {
    help = false;
    version = false;

    for (int i = 1; i < argc; i++) {
      std::string arg = argv[i];
      if (arg == "-h" || arg == "--help") {
        help = true;
      } else if (arg == "-v" || arg == "--version") {
        version = true;
      } else if (arg == "-t" || arg == "--target") {
        if (i + 1 < argc) {
          target = argv[i + 1];
          i++;
        } else {
          std::cerr << "Missing target triple" << std::endl;
          exit(1);
        }
      } else if (arg == "-S") {
        outputFileType = GeneratedFileType::ASSEMBLY;
      } else if (arg == "-c") {
        outputFileType = GeneratedFileType::OBJECT;
      } else if (arg == "-o") {
        if (i + 1 < argc) {
          outputFile = argv[i + 1];
          i++;
        } else {
          std::cerr << "Missing output file" << std::endl;
          exit(1);
        }
      } else if (arg == "--print-ir") {
        printIr = true;
      } else if (arg[0] == '-') {
        std::cerr << "Unknown option: " << arg << std::endl;
        usage(argv[0]);
        exit(1);
      } else {
        file = arg;
      }
    }
    if (outputFile.empty()) {
      outputFile = file;
      if (outputFile.ends_with(".sl")) {
        outputFile = outputFile.substr(0, outputFile.size() - 3);
      }
      if (outputFileType == GeneratedFileType::ASSEMBLY) {
        outputFile += ".s";
      } else {
        outputFile += ".o";
      }
    }
  }
};

int main(int argc, char **argv) {
  Options options(argc, argv);
  if (options.help) {
    help(argv[0]);
  } else if (options.version) {
    std::cout << "Version: " << MAJOR_VERSION << "." << MINOR_VERSION << "."
              << PATCH_VERSION << std::endl;
  } else if (!options.file.empty()) {
    std::ifstream input(options.file);
    Lexer lexer(options.file, input);
    std::unique_ptr<AstNode> ast;
    Parser parser(lexer, options.file, &ast);
    try {
      parser.parse();
      {
        std::map<std::string, const DefinitionNode &> symbolTable;
        ast->assignType(symbolTable, {});
      }
      codegen(*ast, options.target, options.outputFileType, options.outputFile, options.printIr);
    } catch (const SlException &e) {
      std::cerr << e.what() << std::endl;
      exit(1);
    } catch (const std::exception &e) {
      std::cerr << e.what() << std::endl;
      exit(1);
    }
  } else {
    usage(argv[0]);
  }
  return 0;
}
