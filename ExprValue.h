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
  ValueKind mKind;
  union DataFileds {
    std::size_t mStackIndex;
    std::size_t mHeapIndex;
    ValueTy mVal;
    std::size_t mGlobalIndex;
  } mData;

public:
  [[nodiscard]] bool isLValue() const {
    return mKind == ValueKind::REF_STACK || mKind == ValueKind::REF_HEAP ||
           mKind == ValueKind::REF_GLOBAL;
  }

  ExprObject(ValueKind kind, DataFileds data) : mKind(kind), mData(data) {}

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

  ValueKind getKind() const { return mKind; }

  DataFileds getData() const { return mData; }
};
