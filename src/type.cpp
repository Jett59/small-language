#include "type.h"

namespace sl {
    std::string PrimitiveTypeNode::toString() const {
        return primitiveTypeToString[primitiveType];
    }
}
