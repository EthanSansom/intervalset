#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector points_unchop_cpp(const List& chopped, const IntegerVector& sizes) {
  int n = sizes.size();

  if (n == 0) {
    NumericVector out(0);
    return out;
  }

  // Get the total size of the output
  int total_size { 0 };
  for (int i = 0; i < n; i++) {
    if (sizes[i] == NA_INTEGER) {
      total_size++;
    } else {
      total_size += sizes[i];
    }
  }

  // Allocate an output vector
  NumericVector out(total_size);
  NumericVector elm;

  // Unlist `chopped`, assigning elements to `out`
  int index { 0 };
  for (int i = 0; i < n; i++) {
    // In `chopped` these `NA` elements are `NULL`, but want to unlist as `NA`
    if (sizes[i] == NA_INTEGER) {
      out[index] = NA_REAL;
      index++;
      continue;
    }

    // For non-NA elements, unlist as normal
    elm = chopped[i];
    for (int j = 0; j < elm.size(); j++) {
      out[index] = elm[j];
      index++;
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericVector points_unchop_cpp_refined(const List& chopped) {
  int n = chopped.size();
  if (n == 0) return NumericVector(0);

  // 1. Calculate total size internally for safety
  int total_size = 0;
  for (int i = 0; i < n; i++) {
    SEXP el = chopped[i];
    if (Rf_isNull(el)) {
      total_size += 1;
    } else {
      total_size += Rf_length(el);
    }
  }

  // 2. Pre-allocate
  NumericVector out = no_init(total_size);
  int offset = 0;

  // 3. Fill the output
  for (int i = 0; i < n; i++) {
    SEXP el = chopped[i];

    if (Rf_isNull(el)) {
      out[offset] = NA_REAL;
      offset++;
    } else {
      NumericVector nv_el(el); // Wrap the SEXP
      // Use std::copy for faster memory transfer
      std::copy(nv_el.begin(), nv_el.end(), out.begin() + offset);
      offset += nv_el.size();
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericVector list_of_numeric_unchop_cpp(const List& chopped) {
  int n = chopped.size();
  if (n == 0) return NumericVector(0);

  // Calculate the size of the unlisted vector
  size_t total_size = 0;
  for (int i = 0; i < n; i++) {
    SEXP el = chopped[i];
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
  // elements with `NA_real_` in the unlisted output.
  for (int i = 0; i < n; i++) {
    SEXP el = chopped[i];

    if (el == R_NilValue) {
      *p_out = NA_REAL;
      p_out++;
    } else {
      int len = Rf_length(el);
      std::memcpy(p_out, REAL(el), len * sizeof(double));
      p_out += len;
    }
  }

  return out;
}
