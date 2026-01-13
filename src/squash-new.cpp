#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List new_squash_cpp(NumericVector starts, NumericVector ends) {
  int n_spans = starts.size();
  Endpoints endpoints;
  endpoints.reserve(n_spans * 2);

  for (int i = 0; i < n_spans; i++) {
    // TODO: Check your uses of `== NA_REAL` or `== NA_INTEGER` since they aren't always okay.
    //
    // if (starts[i] == NA_REAL || ends[i] == NA_REAL) continue; // This was returning FALSE
    if (NumericVector::is_na(starts[i]) || NumericVector::is_na(ends[i])) continue;
    endpoints.push_back(Endpoint { true, starts[i] });
    endpoints.push_back(Endpoint { false, ends[i] });
  }
  std::sort(endpoints.begin(), endpoints.end());

  // There are at most `n_spans` in the output (when every span in `x` is disjoint)
  std::vector<double> out_starts, out_ends;
  out_starts.reserve(n_spans);
  out_ends.reserve(n_spans);

  // TODO: Revise endpoints so that `is_start` just has a `-1` or `1` which we can
  //       add to the score. Look into notes history for implementation.
  int score = 0;
  for (const Endpoint& endpoint : endpoints) {
    if (endpoint.is_start) {
      if (score == 0) {
        out_starts.push_back(endpoint.value);
      }
      ++score;
    } else {
      if (score == 1) {
        out_ends.push_back(endpoint.value);
      }
      --score;
    }
  }

  NumericVector r_out_starts = wrap(out_starts);
  NumericVector r_out_ends = wrap(out_ends);

  return List::create(
    Named("starts") = r_out_starts,
    Named("ends") = r_out_ends,
    Named("sizes") = IntegerVector::create(r_out_starts.size())
  );
}

// [[Rcpp::export]]
List new_squash_na_cpp(IntegerVector sizes, NumericVector starts, NumericVector ends) {
  int n_spans = starts.size();
  Endpoints endpoints;
  endpoints.reserve(n_spans * 2);

  for (int i { 0 }; i < n_spans; ++i) {
    if (starts[i] == NA_INTEGER || ends[i] == NA_INTEGER) continue;
    endpoints.push_back(Endpoint { true, starts[i] });
    endpoints.push_back(Endpoint { false, ends[i] });
  }
  std::sort(endpoints.begin(), endpoints.end());

  // There are at most `n_spans` in the output (when every span in `x` is disjoint)
  std::vector<double> out_starts, out_ends;
  out_starts.reserve(n_spans);
  out_ends.reserve(n_spans);

  int score = 0;
  for (const Endpoint& endpoint : endpoints) {
    if (endpoint.is_start) {
      if (score == 0) {
        out_starts.push_back(endpoint.value);
      }
      ++score;
    } else {
      if (score == 1) {
        out_ends.push_back(endpoint.value);
      }
      --score;
    }
  }

  NumericVector r_out_starts = wrap(out_starts);
  NumericVector r_out_ends = wrap(out_ends);

  return List::create(
    Named("starts") = r_out_starts,
    Named("ends") = r_out_ends,
    Named("sizes") = IntegerVector::create(starts.size())
  );
}

inline bool is_na(double x) {
  return R_IsNA(x);
}

// [[Rcpp::export]]
List new_squash_2_cpp(NumericVector starts, NumericVector ends, bool has_nas) {
  int n = starts.size();

  const double* p_starts = REAL(starts);
  const double* p_ends = REAL(ends);

  std::vector<int> idx(n);
  std::iota(idx.begin(), idx.end(), 0);

  if (has_nas) {
    std::sort(idx.begin(), idx.end(), [&](int i, int j) {
      double si = p_starts[i];
      double sj = p_starts[j];
      bool na_i = is_na(si);
      bool na_j = is_na(sj);

      // Push NAs to the beginning
      if (na_i && !na_j) return true;
      if (!na_i && na_j) return false;
      if (na_i && na_j) return false;

      if (si != sj) return si < sj;
      return p_ends[i] < p_ends[j];
    });
  } else {
    std::sort(idx.begin(), idx.end(), [&](int i, int j) {
      double si = p_starts[i];
      double sj = p_starts[j];

      if (si != sj) return si < sj;
      return p_ends[i] < p_ends[j];
    });
  }

  NumericVector out_starts = no_init(n);
  NumericVector out_ends = no_init(n);
  double* p_out_s = REAL(out_starts);
  double* p_out_e = REAL(out_ends);
  int count = 0;

  // Skip NA values at the front of the sorted vector
  int i = 0;
  if (has_nas) {
    while (i < n && is_na(p_starts[idx[i]])) {
      i++;
    }
  }

  // TODO: We'll need to hand back an NA set in an `else` statement here
  if (i < n) {
    int first_row = idx[i];
    double current_s = p_starts[first_row];
    double current_max_e = p_ends[first_row];
    i++;

    for (; i < n; ++i) {
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
  }

  return List::create(
    Named("starts") = head(out_starts, count),
    Named("ends") = head(out_ends, count),
    Named("sizes") = IntegerVector::create(count)
  );
}
