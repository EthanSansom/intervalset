#ifndef INTERVALSET_BINARY_OPERATORS_H
#define INTERVALSET_BINARY_OPERATORS_H

#include "SpanBuffer.h"

#include <Rcpp.h>
using namespace Rcpp;

struct Intersect {
  int max_spans(int x_points, int y_points);
  void apply_simple(
      double x_start, double x_end,
      double y_start, double y_end,
      SpanBuffer& out
  );
  void apply(const SpanSetView& x, const SpanSetView& y, SpanBuffer& out);
};

struct Union {
  int max_spans(int x_points, int y_points);
  void apply_simple(
      double x_start, double x_end,
      double y_start, double y_end,
      SpanBuffer& out
  );
  void apply(const SpanSetView& x, const SpanSetView& y, SpanBuffer& out);
};

struct SetDiff {
  int max_spans(int x_points, int y_points);
  void apply_simple(
      double x_start, double x_end,
      double y_start, double y_end,
      SpanBuffer& out
  );
  void apply(const SpanSetView& x, const SpanSetView& y, SpanBuffer& out);
};

#endif
