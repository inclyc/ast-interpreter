#include <cassert>

/// Dereference a pointer, assert it is not null.
template <class T> T &assertDeref(T *ptr) {
  return (assert(ptr && "ptr should not be null!"), *ptr);
}