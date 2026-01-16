#include "v2_SpanSetBuffer.h"
#include <Rcpp.h>
using namespace Rcpp;

SpanSetBuffer::SpanSetBuffer(R_xlen_t n, int reserve_size) {
  size = IntegerVector(no_init(n));
  starts = List(no_init(n));
  ends = List(no_init(n));
  p_size = INTEGER(size);

  // TODO: Test if it's worthwhile to adjust the reserve size
  // - For all set operations, we can calculate the maximum element size
  //   using some function `f(max(x_sizes), max(y_sizes))`.
  temp_starts.reserve(reserve_size);
  temp_ends.reserve(reserve_size);
}

void SpanSetBuffer::add_na_element() {
  p_size[current_elt] = NA_INTEGER;
  SET_VECTOR_ELT(starts, current_elt, R_NilValue);
  SET_VECTOR_ELT(ends, current_elt, R_NilValue);
  current_elt++;
}

void SpanSetBuffer::add_empty_element() {
  p_size[current_elt] = 0;
  SET_VECTOR_ELT(starts, current_elt, Rf_allocVector(REALSXP, 0));
  SET_VECTOR_ELT(ends, current_elt, Rf_allocVector(REALSXP, 0));
  current_elt++;
}

void SpanSetBuffer::add_scalar_element(double start, double end) {
  p_size[current_elt] = 1;
  SET_VECTOR_ELT(starts, current_elt, Rf_ScalarReal(start));
  SET_VECTOR_ELT(ends, current_elt, Rf_ScalarReal(end));
  current_elt++;
}

void SpanSetBuffer::add_span(double start, double end) {
  temp_starts.push_back(start);
  temp_ends.push_back(end);
}

void SpanSetBuffer::finish_element() {
  p_size[current_elt] = temp_starts.size();
  SET_VECTOR_ELT(starts, current_elt, wrap(temp_starts));
  SET_VECTOR_ELT(ends, current_elt, wrap(temp_ends));
  current_elt++;
  temp_starts.clear();
  temp_ends.clear();
}

List SpanSetBuffer::get_results() {
  return List::create(
    Named("size") = size,
    Named("starts") = starts,
    Named("ends") = ends
  );
}
