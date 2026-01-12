#include "binary_operators.h"
#include "SpanBuffer.h"

#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

int Intersect::max_spans(int x_points, int y_points) {
  return std::max(x_points, y_points);
}

void Intersect::apply_simple(
    double x_start, double x_end,
    double y_start, double y_end,
    SpanBuffer& out
) {
  double start { std::max(x_start, y_start) };
  double end { std::min(x_end, y_end) };
  if (end < start) {
    out.add_empty_element();
  } else {
    out.add_scalar_element(start, end);
  }
};

void Intersect::apply(const SpanSetView& x, const SpanSetView& y, SpanBuffer& out) {
  if (x.is_empty() || y.is_empty()) {
    out.add_empty_element();
    return;
  }

  int i = 0, j = 0;
  while (i < x.size && j < y.size) {
    double start = std::max(x.starts[i], y.starts[j]);
    double end = std::min(x.ends[i], y.ends[j]);

    // If `start > end` then x[i] and y[i] don't intersect
    if (start <= end) {
      out.add_span(start, end);
    }

    // If x[i] finishes before y[i]: check x[i + 1] against y[j].
    // Otherwise:                    check x[i] against y[j + 1].
    //
    // Suppose `i = 1`, `j = 1`. We've calculated the x[1] span's intersection
    // with y[1]. Since x[1] ends before y[1], we proceed to x[2]. We know that
    // x[1] doesn't intersect with y[2], ..., y[n] because x and y are ordered.
    // x [ 1 ]  [ 2 ]
    // y   [   1   ]   [ 2 ]
    if (x.ends[i] < y.ends[j]) {
      i++;
    } else {
      j++;
    }
  }

  out.finish_element();
}

// Upper bound is when x and y are disjoint
int Union::max_spans(int x_points, int y_points) {
  return x_points + y_points;
}

// Upper bound is when every span in x (y) punches a hole in a span in y (x)
int SetDiff::max_spans(int x_points, int y_points) {
  return std::max(x_points, y_points) * 2;
}
