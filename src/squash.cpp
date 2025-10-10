#include "endpoint.h"
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// TODO: Think about squashing lists of matrices.
// - do.call(rbind, list_of_matrices) is fast -> maybe just do this?
// - we can make another C++ function specifically for lubridate::intervals
//   where the start and ends are already given
//
// - Need to implement a compliment function, which takes a NumericMatrix
//   and returns another for its compliment
//
// Note, the compliment always requires a matrix with one more row than the
// input set.
// <empty>        -> [-Inf, Inf]                 -> Size 0 -> 1
// [0, 1]         -> [-Inf, 0] and [1, Inf]      -> Size 1 -> 2
// [0, 1], [3, 4] -> [-Inf, 0], [1, 3], [4, Inf] -> Size 2 -> 3
//
// The exception is intervals containing an infinite endpoint.
// [-Inf, 1]      -> [1, Inf]
// [1, Inf]       -> [-Inf, 1]
// [-Inf, Inf]    -> <empty>

// TODO: Can I make this a constant or static?
NumericMatrix na_interval() {
  NumericMatrix matrix(1, 2);
  matrix(0, 0) = NA_REAL;
  matrix(0, 1) = NA_REAL;
  return matrix;
}

NumericMatrix infinite_interval() {
  NumericMatrix matrix(1, 2);
  matrix(0, 0) = -INFINITY;
  matrix(0, 1) = INFINITY;
  return matrix;
}

// Assumes `endpoints` are sorted
NumericMatrix squash(const Endpoints& endpoints) {
  // There are at most `endpoints.size() / 2` intervals - the case where
  // every interval is disjoint.
  std::vector<double> starts, ends;
  starts.reserve(endpoints.size() / 2);
  ends.reserve(endpoints.size() / 2);

  int score { 0 };
  for (const Endpoint& endpoint : endpoints) {
    if (endpoint.is_start) {
      // We're at the first start of a union
      if (score == 0) {
        starts.push_back(endpoint.value);
      }
      ++score;
    } else {
      // We're at the last end of a union
      if (score == 1) {
        ends.push_back(endpoint.value);
      }
      --score;
    }
  }

  int n = starts.size();
  NumericMatrix out(n, 2);
  std::copy(starts.begin(), starts.end(), out.begin());
  std::copy(ends.begin(), ends.end(), out.begin() + n);

  return out;
}

// [[Rcpp::export]]
NumericMatrix squash_interval_set(NumericMatrix x, bool na_rm) {
  int nx { x.nrow() };
  if (nx <= 1) return x;

  // TODO: Make into function? Could do `push_endpoints()` to push the matrix
  //      points into the list.
  std::vector<Endpoint> endpoints;
  endpoints.reserve(nx * 2);

  for (int i { 0 }; i < nx; ++i) {
    // We can assume that if either column of `x` is NA, then both are NA
    if (na_rm && ISNA(x[i])) continue;
    if (!na_rm && ISNA(x[i])) return na_interval();

    endpoints.push_back(Endpoint { true, x[i] });
    endpoints.push_back(Endpoint { false, x[i + nx] });
  }

  // Case where `na_rm` is true and we've only encountered NA values
  if (endpoints.empty()) {
    return na_interval();
  }

  std::sort(endpoints.begin(), endpoints.end());
  return squash(endpoints);
}

// [[Rcpp::export]]
NumericMatrix union_interval_set(NumericMatrix x, NumericMatrix y) {
  // NA values are represented by a 1 x 2 matrix of NAs
  if (ISNA(x[0])) return x;
  if (ISNA(y[0])) return y;

  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0) return y;
  if (ny == 0) return x;

  std::vector<Endpoint> endpoints;
  endpoints.reserve((nx + ny) * 2);

  for (int i { 0 }; i < nx; ++i) {
    endpoints.push_back(Endpoint { true, x[i] });
    endpoints.push_back(Endpoint { false, x[i + nx] });
  }
  for (int i { 0 }; i < ny; ++i) {
    endpoints.push_back(Endpoint { true, y[i] });
    endpoints.push_back(Endpoint { false, y[i + ny] });
  }

  std::sort(endpoints.begin(), endpoints.end());
  return squash(endpoints);
}

// [[Rcpp::export]]
NumericMatrix compliment_interval_set(NumericMatrix x) {
  int n { x.nrow() };
  if (n == 0) return infinite_interval();
  if (ISNA(x[0])) return x;

  // The size of the compliment is n + 1 intervals, unless the start/end is
  // unbounded (infinite).
  bool left_open { x[0] == R_NegInf || x[0] == R_PosInf };
  bool right_open { x[x.size() - 1] == R_NegInf || x[x.size() - 1] == R_PosInf };
  int n_out { n + 1 - left_open - right_open };

  NumericMatrix out(n_out, 2);

  Rcout << "n_out.size() = " << n_out * 2 << "\n";

  for (int i { 0 }; i < n; ++i) {

    Rcout << "i = " << i << "\n";
    Rcout << "n_out - (n - i - right_open) = " << n_out - (n - i - right_open) << "\n";
    Rcout << "n_out + (i - left_open) = " << n_out + (i - left_open) << "\n";

    if (!(right_open && i >= n - 1)) {
      out[n_out - (n - i - right_open)] = x[i + n];
    } else {
      Rcout << "Skipped right open\n";
    }
    if (!(left_open && i <= 0)){
      out[n_out + (i - left_open)] = x[i];
    } else {
      Rcout << "Skipped left open\n";
    }
  }

  if (!left_open) out[0] = R_NegInf;
  if (!right_open) out[out.size() - 1] = R_PosInf;

  return out;
}

// [[Rcpp::export]]
NumericVector inf() {
  NumericVector out(1);
  out[0] = INFINITY;
  return out;
}
