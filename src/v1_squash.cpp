#include <algorithm>
#include <numeric>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List squash_v1_cpp(NumericVector starts, NumericVector ends) {
  int n = starts.size();

  const double* p_starts = REAL(starts);
  const double* p_ends = REAL(ends);

  std::vector<int> idx(n);
  std::iota(idx.begin(), idx.end(), 0);

  // TODO: For custom within-group squash, sort on the group index as well,
  //       that way we can sort the entire vector on the group index.
  std::sort(idx.begin(), idx.end(), [&](int i, int j) {
    double si = p_starts[i];
    double sj = p_starts[j];

    if (si != sj) return si < sj;
    return p_ends[i] < p_ends[j];
  });

  // TODO: For the by-group approach, this remains the same since the number of
  //       potential spans is unchanged.
  //
  // TODO: We'll need to allocate an `out_sizes`. This will depend on the number
  //       of unique groups, which is just the maximum of `vctrs::vec_duplicate_id(groups)`.
  //       Pass the maximum in as a parameter to this function.
  NumericVector out_starts = no_init(n);
  NumericVector out_ends = no_init(n);
  double* p_out_s = REAL(out_starts);
  double* p_out_e = REAL(out_ends);
  int count = 0;

  int row = idx[0];
  double current_s = p_starts[row];
  double current_max_e = p_ends[row];

  // TODO: Here, we'll have to reset the first `current_s` and `current_max_e`
  //       every time we move onto a new group. We'll also record `count` within
  //       groups so we can record the size each time.
  for (int i = 1; i < n; ++i) {
    int row = idx[i];
    double next_s = p_starts[row];
    double next_e = p_ends[row];

    if (next_s <= current_max_e) {
      if (next_e > current_max_e) {
        current_max_e = next_e;
      }
    } else {
      p_out_s[count] = current_s;
      p_out_e[count] = current_max_e;
      count++;

      current_s = next_s;
      current_max_e = next_e;
    }
  }

  p_out_s[count] = current_s;
  p_out_e[count] = current_max_e;
  count++;

  return List::create(
    Named("starts") = head(out_starts, count),
    Named("ends") = head(out_ends, count),
    Named("sizes") = IntegerVector::create(count)
  );
}
