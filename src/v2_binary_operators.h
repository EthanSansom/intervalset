#ifndef INTERVALSET_V2_BINARY_OPERATORS_H
#define INTERVALSET_V2_BINARY_OPERATORS_H

#include "v2_SpanSetBuffer.h"
#include "v2_SpanSetVector.h"
#include <Rcpp.h>
using namespace Rcpp;

struct Intersect {
  void apply_simple(const SpanSetView& x, const SpanSetView& y, SpanSetBuffer& out);
  void apply(const SpanSetView& x, const SpanSetView& y, SpanSetBuffer& out);
};

#endif
