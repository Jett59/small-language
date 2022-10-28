#ifndef SL_TYPE_H
#define SL_TYPE_H

#include <map>
#include <memory>
#include <string>
#include <vector>

namespace sl {
enum class TypeType { PRIMITIVE, REFERENCE, FUNCTION, ARRAY };
class Type {
public:
  TypeType type;
  Type(TypeType type) : type(type) {}
  virtual ~Type() = default;

  virtual std::string toString() const = 0;

  virtual std::unique_ptr<Type> clone() const = 0;

  virtual bool equals(const Type &other) const = 0;
};

struct NameAndType {
  std::string name;
  std::unique_ptr<Type> type;

  NameAndType(std::string name, std::unique_ptr<Type> type)
      : name(std::move(name)), type(std::move(type)) {}
};

enum class PrimitiveType {
  NIL,
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
};

static inline bool isSigned(PrimitiveType type) {
  switch (type) {
  case PrimitiveType::I8:
  case PrimitiveType::I16:
  case PrimitiveType::I32:
  case PrimitiveType::I64:
  case PrimitiveType::IPTR:
    return true;
  default:
    return false;
  }
}
static inline bool isIntegral(PrimitiveType type) {
  switch (type) {
  case PrimitiveType::I8:
  case PrimitiveType::I16:
  case PrimitiveType::I32:
  case PrimitiveType::I64:
  case PrimitiveType::IPTR:
  case PrimitiveType::U8:
  case PrimitiveType::U16:
  case PrimitiveType::U32:
  case PrimitiveType::U64:
  case PrimitiveType::UPTR:
    return true;
  default:
    return false;
  }
}
static inline bool isFloat(PrimitiveType type) {
  switch (type) {
  case PrimitiveType::F32:
  case PrimitiveType::F64:
    return true;
  default:
    return false;
  }
}

static std::map<PrimitiveType, std::string> primitiveTypeToString = {
    {PrimitiveType::NIL, "nil"},   {PrimitiveType::I8, "i8"},
    {PrimitiveType::I16, "i16"},   {PrimitiveType::I32, "i32"},
    {PrimitiveType::I64, "i64"},   {PrimitiveType::IPTR, "iptr"},
    {PrimitiveType::U8, "u8"},     {PrimitiveType::U16, "u16"},
    {PrimitiveType::U32, "u32"},   {PrimitiveType::U64, "u64"},
    {PrimitiveType::UPTR, "uptr"}, {PrimitiveType::F32, "f32"},
    {PrimitiveType::F64, "f64"},   {PrimitiveType::BOOL, "bool"},
    {PrimitiveType::CHAR, "char"}, {PrimitiveType::NIL, "nil"},
};
class PrimitiveTypeNode : public Type {
public:
  PrimitiveType primitiveType;
  PrimitiveTypeNode(PrimitiveType primitiveType)
      : Type(TypeType::PRIMITIVE), primitiveType(primitiveType) {}

  std::string toString() const override;

  std::unique_ptr<Type> clone() const override;

  bool equals(const Type &other) const override;
};
static inline bool isSigned(const Type &type) {
  if (type.type != TypeType::PRIMITIVE) {
    return false;
  }
  return isSigned(static_cast<const PrimitiveTypeNode &>(type).primitiveType);
}
static inline bool isIntegral(const Type &type) {
  if (type.type != TypeType::PRIMITIVE) {
    return false;
  }
  return isIntegral(static_cast<const PrimitiveTypeNode &>(type).primitiveType);
}
static inline bool isFloat(const Type &type) {
  if (type.type != TypeType::PRIMITIVE) {
    return false;
  }
  return isFloat(static_cast<const PrimitiveTypeNode &>(type).primitiveType);
}

class ReferenceTypeNode : public Type {
public:
  std::unique_ptr<Type> type;
  bool constant;

  ReferenceTypeNode(std::unique_ptr<Type> type, bool constant)
      : Type(TypeType::REFERENCE), type(std::move(type)), constant(constant) {}

  std::string toString() const override;

  std::unique_ptr<Type> clone() const override;

  bool equals(const Type &other) const override;
};

static inline bool isRefToInteger(const Type &type) {
  if (type.type != TypeType::REFERENCE) {
    return false;
  }
  const auto &refType = static_cast<const ReferenceTypeNode &>(type);
  return isIntegral(*refType.type);
}
static inline bool isRefToFloat(const Type &type) {
  if (type.type != TypeType::REFERENCE) {
    return false;
  }
  const auto &refType = static_cast<const ReferenceTypeNode &>(type);
  return isFloat(*refType.type);
}
static inline bool isRefToSigned(const Type &type) {
  if (type.type != TypeType::REFERENCE) {
    return false;
  }
  const auto &refType = static_cast<const ReferenceTypeNode &>(type);
  return isSigned(*refType.type);
}

class FunctionTypeNode : public Type {
public:
  std::vector<std::unique_ptr<Type>> arguments;
  std::unique_ptr<Type> returnType;

  FunctionTypeNode(std::vector<std::unique_ptr<Type>> arguments,
                   std::unique_ptr<Type> returnType)
      : Type(TypeType::FUNCTION), arguments(std::move(arguments)),
        returnType(std::move(returnType)) {}

  std::string toString() const override;

  std::unique_ptr<Type> clone() const override;

  bool equals(const Type &other) const override;
};
class ArrayType : public Type {
public:
  std::unique_ptr<Type> type;

  ArrayType(std::unique_ptr<Type> type)
      : Type(TypeType::ARRAY), type(std::move(type)) {}

  std::string toString() const override;

  std::unique_ptr<Type> clone() const override;

  bool equals(const Type &other) const override;
};
} // namespace sl

#endif