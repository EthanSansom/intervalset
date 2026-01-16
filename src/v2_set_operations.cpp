#include "v2_binary_operators.h"
#include "v2_SpanSetVector.h"
#include "v2_vectorization.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List intersect_v2_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends
) {
  const SpanSetVector x = { x_size, x_starts, x_ends };
  const SpanSetVector y = { y_size, y_starts, y_ends };
  const Intersect intersect;
  return v2_map_binary_op(x, y, intersect);
}
