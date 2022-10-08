#include <iostream>

void usage(char *program) {
  std::cout << "Usage: " << program << " [options] file" << std::endl;
}

void help(char *program) {
  usage(program);
  std::cout << "Options:" << std::endl;
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

    Options(int argc, char **argv) {
        help = false;
        version = false;

        for (int i = 1; i < argc; i++) {
            std::string arg = argv[i];
            if (arg == "-h" || arg == "--help") {
                help = true;
            } else if (arg == "-v" || arg == "--version") {
                version = true;
            }else if (arg[0] == '-') {
                std::cerr << "Unknown option: " << arg << std::endl;
                usage(argv[0]);
                exit(1);
            } else{
                file = arg;
            }
        }
    }
    };

int main(int argc, char **argv) {
  Options options(argc, argv);
  if (options.help) {
    help(argv[0]);
  }else if (options.version) {
    std::cout << "Version: " << MAJOR_VERSION << "." << MINOR_VERSION << "." << PATCH_VERSION << std::endl;
  }else if (!options.file.empty()) {
    std::cout << "File: " << options.file << std::endl;
  }else {
    usage(argv[0]);
  }
  return 0;
}
