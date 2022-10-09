#ifndef SL_TYPE_H
#define SL_TYPE_H

#include <memory>
#include <string>

namespace sl {
enum class TypeType { PRIMITIVE };
class Type {
public:
  TypeType type;
  Type(TypeType type) : type(type) {}
  virtual ~Type() = default;
};

struct NameAndType {
  std::string name;
  std::unique_ptr<Type> type;
};

enum class PrimitiveType {
  I8,
  I16,
  I32,
  I64,
  IPTR,
  U8,
  U16,
  U32,
  U64,
  UPTR,
  F32,
  F64,
  BOOL,
  CHAR,
  STRING,
};
class PrimitiveTypeNode : public Type {
public:
  PrimitiveType primitiveType;
  PrimitiveTypeNode(PrimitiveType primitiveType)
      : Type(TypeType::PRIMITIVE), primitiveType(primitiveType) {}
};
} // namespace sl

#endif