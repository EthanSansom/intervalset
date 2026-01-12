#ifndef INTERVALSET_SPANSETVECTOR_H
#define INTERVALSET_SPANSETVECTOR_H

#include "SpanBuffer.h"

#include <Rcpp.h>
using namespace Rcpp;

struct SpanSetVector {
  const IntegerVector& sizes;
  const NumericVector& starts;
  const NumericVector& ends;

  int n_sets() const { return sizes.size(); }
  int n_spans() const { return starts.size(); }

  SpanSetView view(int i, int offset) const {
    return { &starts[offset], &ends[offset], sizes[i] };
  }
};

#endif
