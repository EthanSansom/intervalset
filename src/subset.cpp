#include "v1_SpanSetVector.h"
#include "v1_SpanBuffer.h"
#include "v1_utils.h"

#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List subset_cpp(IntegerVector sizes, NumericVector starts, NumericVector ends, IntegerVector indices) {
  int n_sets = sizes.size();

  // Calculate the number of output spans and the offset to each set
  std::vector<int> offsets(n_sets);
  int current_offset = 0;
  for (int i = 0; i < n_sets; i++) {
    int size = sizes[i];
    offsets[i] = current_offset;
    current_offset += (size == NA_INTEGER) ? 1 : size;
  }

  // Create the output buffer
  IntegerVector out_sizes = sizes[indices - 1]; // `indices` from R are 1-indexed

  int n_sets_out = indices.size();
  int n_spans_out = 0;
  for (int i = 0; i < n_sets_out; i++) {
    int size = out_sizes[i];
    n_spans_out += (size == NA_INTEGER) ? 1 : size;
  }

  NumericVector out_starts = no_init(n_spans_out);
  NumericVector out_ends = no_init(n_spans_out);

  // Copy starts/ends using the offsets
  int out_offset = 0;
  for (int i = 0; i < n_sets_out; i++) {
    int offset = offsets[indices[i] - 1]; // `indices` from R are 1-indexed
    int size = (out_sizes[i] == NA_INTEGER) ? 1 : out_sizes[i];

    r_memcpy(&out_starts[out_offset], &starts[offset], size * sizeof(double));
    r_memcpy(&out_ends[out_offset], &ends[offset], size * sizeof(double));

    out_offset += size;
  }

  return List::create(
    Named("starts") = out_starts,
    Named("ends") = out_ends,
    Named("sizes") = out_sizes
  );
}
