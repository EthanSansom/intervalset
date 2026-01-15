#ifndef INTERVALSET_V1_UTILS_H
#define INTERVALSET_V1_UTILS_H

#include <cstddef>
#include <cstring>

// https://github.com/r-lib/rlang/blob/1df507ece8c3f2a58ebfe9a252e855de702e8831/src/rlang/c-utils.h#L180
static inline
void r_memcpy(void* dest, const void* src, std::size_t count) {
  if (count) {
    std::memcpy(dest, src, count);
  }
}

#endif
