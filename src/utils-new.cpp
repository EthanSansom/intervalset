#include <Rcpp.h>
using namespace Rcpp;

// https://github.com/r-lib/rlang/blob/1df507ece8c3f2a58ebfe9a252e855de702e8831/src/rlang/c-utils.h#L180
static inline
void r_memcpy(void* dest, const void* src, size_t count) {
  if (count) {
    std::memcpy(dest, src, count);
  }
}

// [[Rcpp::export]]
NumericVector listof_dbl_unchop_cpp(const List& chopped) {
  int n = chopped.size();
  if (n == 0) return NumericVector(0);

  size_t total_size = 0;
  for (int i = 0; i < n; i++) {
    SEXP el = VECTOR_ELT(chopped, i);
    if (el == R_NilValue) {
      total_size++;
    } else {
      if (TYPEOF(el) != REALSXP) {
        stop("Element at index %i is not a numeric vector or `NULL`.", i + 1);
      }
      total_size += Rf_length(el);
    }
  }

  NumericVector out = no_init(total_size);
  double* p_out = REAL(out);

  // Unlist `chopped` into `out`. Unlike vec_unchop(), we replace `NULL`
  // elements with `NA_real_` in the unlisted output. The type of `chopped`
  // elements has been confirmed as either `NULL` or numeric in the prior loop.
  for (int i = 0; i < n; i++) {
    SEXP el = VECTOR_ELT(chopped, i);
    if (el == R_NilValue) {
      *p_out = NA_REAL;
      p_out++;
    } else {
      int len = Rf_length(el);
      r_memcpy(p_out, REAL(el), len * sizeof(double));
      p_out += len;
    }
  }

  return out;
}
