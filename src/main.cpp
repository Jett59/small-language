#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "error.h"
#include <fstream>
#include <iostream>

using namespace sl;

static void usage(char *program) {
  std::cout << "Usage: " << program << " [options] file" << std::endl;
}

static void help(char *program) {
  usage(program);
  std::cout << "Options:" << std::endl;
  std::cout << "-t, --target <triple>\t\tTarget triple" << std::endl;
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

  Options(int argc, char **argv) {
    help = false;
    version = false;

    for (int i = 1; i < argc; i++) {
      std::string arg = argv[i];
      if (arg == "-h" || arg == "--help") {
        help = true;
      } else if (arg == "-v" || arg == "--version") {
        version = true;
      }else if (arg == "-t" || arg == "--target") {
        if (i + 1 < argc) {
          target = argv[i + 1];
          i++;
        } else {
          std::cerr << "Missing target triple" << std::endl;
          exit(1);
        }
      } else if (arg[0] == '-') {
        std::cerr << "Unknown option: " << arg << std::endl;
        usage(argv[0]);
        exit(1);
      } else {
        file = arg;
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
    Parser parser(lexer);
    try {
      auto ast = parser.parse();
      {
        std::map<std::string, const DefinitionNode &> symbolTable;
        ast->assignType(symbolTable);
      }
      codegen(*ast, options.target);
    } catch (const SlException &e) {
      std::cerr << e.what() << std::endl;
      exit(1);
    }catch (const std::exception &e) {
      std::cerr << e.what() << std::endl;
      exit(1);
    }
  } else {
    usage(argv[0]);
  }
  return 0;
}
