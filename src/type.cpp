#include "type.h"

namespace sl {
using std::string_literals::operator""s;

std::string PrimitiveTypeNode::toString() const {
  return primitiveTypeToString[primitiveType];
}
std::string ReferenceTypeNode::toString() const {
  return "&"s + (constant ? ""s : "mut"s) + type->toString();
}
} // namespace sl
