#ifndef SL_TYPE_H
#define SL_TYPE_H

#include <map>
#include <memory>
#include <string>

namespace sl {
enum class TypeType { PRIMITIVE, REFERENCE };
class Type {
public:
  TypeType type;
  Type(TypeType type) : type(type) {}
  virtual ~Type() = default;

  virtual std::string toString() const = 0;
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
  STRING,
};
static std::map<PrimitiveType, std::string> primitiveTypeToString = {
    {PrimitiveType::NIL, "nil"},   {PrimitiveType::I8, "i8"},
    {PrimitiveType::I16, "i16"},   {PrimitiveType::I32, "i32"},
    {PrimitiveType::I64, "i64"},   {PrimitiveType::IPTR, "iptr"},
    {PrimitiveType::U8, "u8"},     {PrimitiveType::U16, "u16"},
    {PrimitiveType::U32, "u32"},   {PrimitiveType::U64, "u64"},
    {PrimitiveType::UPTR, "uptr"}, {PrimitiveType::F32, "f32"},
    {PrimitiveType::F64, "f64"},   {PrimitiveType::BOOL, "bool"},
    {PrimitiveType::CHAR, "char"}, {PrimitiveType::STRING, "string"},
};
class PrimitiveTypeNode : public Type {
public:
  PrimitiveType primitiveType;
  PrimitiveTypeNode(PrimitiveType primitiveType)
      : Type(TypeType::PRIMITIVE), primitiveType(primitiveType) {}

  std::string toString() const override;
};
class ReferenceTypeNode : public Type {
public:
  std::unique_ptr<Type> type;
  bool constant;

  ReferenceTypeNode(std::unique_ptr<Type> type, bool constant)
      : Type(TypeType::REFERENCE), type(std::move(type)), constant(constant) {}

  std::string toString() const override;
};
} // namespace sl

#endif