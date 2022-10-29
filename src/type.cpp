#include "type.h"

namespace sl {
using std::string_literals::operator""s;

std::string PrimitiveTypeNode::toString() const {
  return primitiveTypeToString[primitiveType];
}
std::string ReferenceTypeNode::toString() const {
  return "&"s + (constant ? ""s : "mut "s) + type->toString();
}
std::string FunctionTypeNode::toString() const {
  std::string result;
  result = "("s;
  for (const auto &argument : arguments) {
    result += argument->toString() + ", "s;
  }
  result = result.substr(0, result.size() - 2);
  result += ")->" + returnType->toString();
  return result;
}
std::string ArrayType::toString() const {
  return "["s + type->toString() + "]"s;
}

std::unique_ptr<Type> PrimitiveTypeNode::clone() const {
  return std::make_unique<PrimitiveTypeNode>(primitiveType);
}
std::unique_ptr<Type> ReferenceTypeNode::clone() const {
  return std::make_unique<ReferenceTypeNode>(type->clone(), constant);
}
std::unique_ptr<Type> FunctionTypeNode::clone() const {
  std::vector<std::unique_ptr<Type>> clonedArguments;
  for (const auto &argument : arguments) {
    clonedArguments.push_back(argument->clone());
  }
  return std::make_unique<FunctionTypeNode>(std::move(clonedArguments),
                                            returnType->clone());
}
std::unique_ptr<Type> ArrayType::clone() const {
  return std::make_unique<ArrayType>(type->clone());
}

bool PrimitiveTypeNode::equals(const Type &other) const {
  if (other.type != TypeType::PRIMITIVE) {
    return false;
  }
  return primitiveType ==
         static_cast<const PrimitiveTypeNode &>(other).primitiveType;
}
bool ReferenceTypeNode::equals(const Type &other) const {
  if (other.type != TypeType::REFERENCE) {
    return false;
  }
  const auto &otherReferenceType =
      static_cast<const ReferenceTypeNode &>(other);
  return constant == otherReferenceType.constant &&
         type->equals(*otherReferenceType.type);
}
bool FunctionTypeNode::equals(const Type &other) const {
  if (other.type != TypeType::FUNCTION) {
    return false;
  }
  const auto &otherFunctionType = static_cast<const FunctionTypeNode &>(other);
  if (arguments.size() != otherFunctionType.arguments.size()) {
    return false;
  }
  for (size_t i = 0; i < arguments.size(); i++) {
    if (!arguments[i]->equals(*otherFunctionType.arguments[i])) {
      return false;
    }
  }
  return returnType->equals(*otherFunctionType.returnType);
}
bool ArrayType::equals(const Type &other) const {
  if (other.type != TypeType::ARRAY) {
    return false;
  }
  return type->equals(*static_cast<const ArrayType &>(other).type);
}
} // namespace sl
