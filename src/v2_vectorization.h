#ifndef INTERVALSET_V2_VECTORIZATION_H
#define INTERVALSET_V2_VECTORIZATION_H

#include "v2_SpanSetVector.h"
#include "v2_SpanSetBuffer.h"
#include <Rcpp.h>
using namespace Rcpp;

template <typename Op>
List v2_map_binary_op(const SpanSetVector& x, const SpanSetVector& y, Op op) {
  const R_xlen_t xn = x.n_sets();
  const R_xlen_t yn = y.n_sets();

  // if (xn == 1) return map_binary_op_recycled(y, x.view(0, 0), op);
  // if (yn == 1) return map_binary_op_recycled(x, y.view(0, 0), op);

  const R_xlen_t n = xn; // Size compatibility is checked in R
  SpanSetBuffer buffer(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int x_size = x.size[i];
    int y_size = y.size[i];

    if (x_size == NA_INTEGER || y_size == NA_INTEGER) {
      buffer.add_na_element();
    } else if (x_size == 1 && y_size == 1) {
      op.apply_simple(x.view(i), y.view(i), buffer);
    } else {
      op.apply(x.view(i), y.view(i), buffer);
    }
  }

  return buffer.get_results();
}

#endif
