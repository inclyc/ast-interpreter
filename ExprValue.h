#pragma once

#include "Support.h"

#include <cstdint>

class ExprObject {
public:
  enum class ValueKind {
    REF_STACK,
    REF_HEAP,
    VAL,
    REF_GLOBAL,
  };

private:
  ValueKind mType;
  union DataFileds {
    std::size_t mStackIndex;
    std::size_t mHeapIndex;
    ValueTy mVal;
    std::size_t mGlobalIndex;
  } mData;

  ExprObject(ValueKind type, DataFileds data) : mType(type), mData(data) {}

public:
  [[nodiscard]] bool isLValue() const {
    return mType == ValueKind::REF_STACK || mType == ValueKind::REF_HEAP ||
           mType == ValueKind::REF_GLOBAL;
  }

  static ExprObject mkRefStack(std::size_t idx) {
    return ExprObject(ExprObject::ValueKind::REF_STACK, {.mStackIndex = idx});
  }

  static ExprObject mkRefHeap(std::size_t idx) {
    return ExprObject(ExprObject::ValueKind::REF_HEAP, {.mHeapIndex = idx});
  }

  static ExprObject mkRefGlobal(std::size_t idx) {
    return ExprObject(ExprObject::ValueKind::REF_GLOBAL, {.mGlobalIndex = idx});
  }

  static ExprObject mkVal(ValueTy val) {
    return ExprObject(ExprObject::ValueKind::VAL, {.mVal = val});
  }

  ValueKind getType() const { return mType; }

  DataFileds getData() const { return mData; }
};
