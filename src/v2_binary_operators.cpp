#include "v2_binary_operators.h"
#include "v2_SpanSetBuffer.h"
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

// -----------------------------------------------------------------------------

// TODO: Future functions might look like
void interval_intersect(
    double x_start, double x_end,
    double y_start, double y_end,
    SpanSetBuffer& out
) {
  double start { std::max(x_start, y_start) };
  double end { std::min(x_end, y_end) };
  if (end < start) {
    out.add_empty_element();
  } else {
    out.add_scalar_element(start, end);
  }
}

void phinterval_intersect(
    const SpanSetView& x,
    const SpanSetView& y,
    SpanSetBuffer& out
) {
  // Intersect::apply()
}

void phinterval_intersect1(
    const SpanSetView& x,
    const SpanSetView& y,
    SpanSetBuffer& out
) {
  // Calls interval_intersect()
}

// -----------------------------------------------------------------------------

void Intersect::apply_simple(
    const SpanSetView& x,
    const SpanSetView& y,
    SpanSetBuffer& out
) {
  double start { std::max(x.starts[0], y.starts[0]) };
  double end { std::min(x.ends[0], y.ends[0]) };
  if (end < start) {
    out.add_empty_element();
  } else {
    out.add_scalar_element(start, end);
  }
};

void Intersect::apply(
    const SpanSetView& x,
    const SpanSetView& y,
    SpanSetBuffer& out
) {
  if (x.is_empty() || y.is_empty()) {
    out.add_empty_element();
    return;
  }

  int i = 0, j = 0;
  while (i < x.size && j < y.size) {
    double start = std::max(x.starts[i], y.starts[j]);
    double end = std::min(x.ends[i], y.ends[j]);

    if (start <= end) {
      out.add_span(start, end);
    }

    if (x.ends[i] < y.ends[j]) {
      i++;
    } else {
      j++;
    }
  }

  out.finish_element();
}
