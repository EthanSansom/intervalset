#include "vectorization.h"
#include "binary_operators.h"

#include "SpanBuffer.h" // TEMP

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List intersect_cpp(
    const IntegerVector& x_size, const NumericVector& x_starts, const NumericVector& x_ends,
    const IntegerVector& y_size, const NumericVector& y_starts, const NumericVector& y_ends
) {
  const SpanSetVector x = { x_size, x_starts, x_ends };
  const SpanSetVector y = { y_size, y_starts, y_ends };
  const Intersect intersect;
  return map_binary_op(x, y, intersect);
}

// [[Rcpp::export]]
List span_set_only_cpp(
    const IntegerVector& x_size, const NumericVector& x_starts, const NumericVector& x_ends,
    const IntegerVector& y_size, const NumericVector& y_starts, const NumericVector& y_ends
) {
  const SpanSetVector x = { x_size, x_starts, x_ends };
  const SpanSetVector y = { y_size, y_starts, y_ends };
  const Intersect intersect;
  return List(0);
}

// [[Rcpp::export]]
List do_nothing_cpp(
    const IntegerVector& x_size, const NumericVector& x_starts, const NumericVector& x_ends,
    const IntegerVector& y_size, const NumericVector& y_starts, const NumericVector& y_ends
) {
  // const SpanSetVector x = { x_size, x_starts, x_ends };
  // const SpanSetVector y = { y_size, y_starts, y_ends };
  // const Intersect intersect;
  return List(0);
}

// [[Rcpp::export]]
List span_buffer_only_cpp(
    const IntegerVector& x_size, const NumericVector& x_starts, const NumericVector& x_ends,
    const IntegerVector& y_size, const NumericVector& y_starts, const NumericVector& y_ends
) {
  // const SpanSetVector x = { x_size, x_starts, x_ends };
  // const SpanSetVector y = { y_size, y_starts, y_ends };
  // Intersect intersect;

  SpanBuffer buffer(x_size.size(), std::max(x_starts.size(), y_starts.size()));

  return List(0);
}
