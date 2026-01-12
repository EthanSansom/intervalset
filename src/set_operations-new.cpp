#include "vectorization.h"
#include "binary_operators.h"

#include "SpanBuffer.h" // TEMP

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List new_intersect_cpp(
    IntegerVector x_size, NumericVector x_starts, NumericVector x_ends,
    IntegerVector y_size, NumericVector y_starts, NumericVector y_ends
) {
  const SpanSetVector x = { x_size, x_starts, x_ends };
  const SpanSetVector y = { y_size, y_starts, y_ends };
  const Intersect intersect;
  return map_binary_op(x, y, intersect);
}
