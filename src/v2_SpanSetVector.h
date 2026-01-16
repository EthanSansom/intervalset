#ifndef INTERVALSET_SPANSETVECTOR_V2_H
#define INTERVALSET_SPANSETVECTOR_V2_H

#include <Rcpp.h>
using namespace Rcpp;

struct SpanSetView {
  const int size;
  const double* starts;
  const double* ends;

  bool is_empty() const { return !size; }
};

class SpanSetVector {
public:
  const IntegerVector& size;
  const List& starts;
  const List& ends;
  const int* p_size;

  SpanSetVector(
    const IntegerVector& size_,
    const List& starts_,
    const List& ends_
  ) : size(size_), starts(starts_), ends(ends_), p_size(INTEGER(size_)) {}

  SpanSetView view(R_xlen_t i) const {
    SEXP starts_i = VECTOR_ELT(starts, i);
    SEXP ends_i = VECTOR_ELT(ends, i);
    return { p_size[i], REAL(starts_i), REAL(ends_i) };
  }
  R_xlen_t n_sets() const { return size.size(); }
};

#endif
