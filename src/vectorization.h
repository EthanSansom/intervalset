#ifndef INTERVALSET_VECTORIZATION_H
#define INTERVALSET_VECTORIZATION_H

#include "SpanSetVector.h"
#include "SpanBuffer.h"

#include <Rcpp.h>
using namespace Rcpp;

template <typename Op>
List map_binary_op_recycled(const SpanSetVector& x, const SpanSetView& y, Op op) {
  const int n = x.n_sets();
  SpanBuffer buffer(n, op.max_spans(x.n_spans(), 1));
  int x_span = 0;

  const int y_size = y.size;
  const double y_start = *y.starts;
  const double y_end = *y.ends;
  const bool y_na { y_size == NA_INTEGER };
  const bool y_scalar { y_size == 1 };

  for (int i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int x_size = x.sizes[i];

    if (y_na || x_size == NA_INTEGER) {
      buffer.add_na_element();
    } else if (y_scalar && x_size == 1) {
      op.apply_simple(x.starts[x_span], x.ends[x_span], y_start, y_end, buffer);
    } else {
      op.apply(x.view(i, x_span), y, buffer);
    }
    x_span += (x_size == NA_INTEGER ? 1 : x_size);
  }

  return buffer.get_results();
}

template <typename Op>
List map_binary_op(const SpanSetVector& x, const SpanSetVector& y, Op op) {
  const int xn = x.n_sets();
  const int yn = y.n_sets();

  if (xn == 1) return map_binary_op_recycled(y, x.view(0, 0), op);
  if (yn == 1) return map_binary_op_recycled(x, y.view(0, 0), op);

  const int n = xn; // Size compatibility of `x` and `y` are checked in R
  SpanBuffer buffer(n, op.max_spans(x.n_spans(), y.n_spans()));
  int x_span = 0, y_span = 0;

  for (int i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int x_size = x.sizes[i];
    int y_size = y.sizes[i];

    if (x_size == NA_INTEGER || y_size == NA_INTEGER) {
      buffer.add_na_element();
    } else if (x_size == 1 && y_size == 1) {
      op.apply_simple(x.starts[x_span], x.ends[x_span], y.starts[y_span], y.ends[y_span], buffer);
    } else {
      op.apply(x.view(i, x_span), y.view(i, y_span), buffer);
    }
    x_span += (x_size == NA_INTEGER ? 1 : x_size);
    y_span += (y_size == NA_INTEGER ? 1 : y_size);
  }

  return buffer.get_results();
}

template <typename Rel>
LogicalVector map_binary_rel(const SpanSetVector& x, const SpanSetVector& y, Rel rel);

template <typename Rel>
LogicalVector map_binary_rel_recycled(const SpanSetVector& x, const SpanSetVector& y, Rel rel);

#endif
