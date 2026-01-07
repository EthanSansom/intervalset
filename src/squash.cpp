#include "endpoint.h"
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// Assumes endpoints are sorted:
// - squash()
// - setdiff()
// - intersect()
// - overlaps()

// TODO:
// - Only export the vectorized versions of each function

// utils -----------------------------------------------------------------------

NumericMatrix na_interval() {
  NumericMatrix matrix(1, 2);
  matrix(0, 0) = NA_REAL;
  matrix(0, 1) = NA_REAL;
  return matrix;
}

NumericMatrix infinite_interval() {
  NumericMatrix matrix(1, 2);
  matrix(0, 0) = R_NegInf;
  matrix(0, 1) = R_PosInf;
  return matrix;
}

// squash ----------------------------------------------------------------------

NumericMatrix squash(const Endpoints& endpoints) {
  // There are at most `endpoints.size() / 2` intervals in the case where every
  // interval is disjoint.
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

// TODO: Call this squash_matrix() since the matrix is arbitrary, can include
// NA or NaN values at any point.
// [[Rcpp::export]]
NumericMatrix squash_interval_set(NumericMatrix x, bool na_rm) {
  int n { x.nrow() };
  if (n <= 1) return x;

  Endpoints endpoints;
  endpoints.reserve(n * 2);

  for (int i { 0 }; i < n; ++i) {
    if (na_rm && ISNA(x[i])) continue;
    if (!na_rm && ISNA(x[i])) return na_interval();

    endpoints.push_back(Endpoint { true, x[i] });
    endpoints.push_back(Endpoint { false, x[i + n] });
  }

  // Case where `na_rm` and we've only encountered NA values
  if (endpoints.empty()) return na_interval();

  std::sort(endpoints.begin(), endpoints.end());
  return squash(endpoints);
}

// union -----------------------------------------------------------------------

// [[Rcpp::export]]
NumericMatrix union_interval_set(NumericMatrix x, NumericMatrix y) {
  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0) return y;
  if (ny == 0) return x;
  if (ISNA(x[0])) return x;
  if (ISNA(y[0])) return y;

  Endpoints endpoints;
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
List v_union_interval_set(const List& x, const List& y) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = union_interval_set(x[i], y[i]);
  }
  return out;
}

// compliment ------------------------------------------------------------------

// We can take the compliment by populating the output matrix with endpoints of
// `x` shifted such that starts become ends and vice-versa. Some care must be
// taken to handle infinite endpoints.
//
// Bounded                Unbounded (Left)         Unbounded (Both)
// [a, b] -> [-Inf, a]    [-Inf, b] -> [b, c]      [-Inf, b] -> [b, c]
// [c, d]    [b,    c]    [c,    d]    [d, Inf]    [c,  Inf]
//           [d,  Inf]
// [[Rcpp::export]]
NumericMatrix compliment_interval_set(NumericMatrix x) {
  int n { x.nrow() };
  if (n == 0) return infinite_interval();
  if (ISNA(x[0])) return x;

  bool left_open { x[0] == R_NegInf || x[0] == R_PosInf };
  bool right_open { x[x.size() - 1] == R_NegInf || x[x.size() - 1] == R_PosInf };
  int n_out { n + 1 - left_open - right_open };

  // These conditions were found by trial and error
  NumericMatrix out(n_out, 2);
  for (int i { 0 }; i < n; ++i) {
    if (!(right_open && i >= n - 1)) out[i + 1 - left_open] = x[i + n];
    if (!(left_open && i <= 0))      out[n_out + i - left_open] = x[i];
  }
  if (!left_open) out[0] = R_NegInf;
  if (!right_open) out[out.size() - 1] = R_PosInf;

  return out;
}

// [[Rcpp::export]]
List v_compliment_interval_set(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = compliment_interval_set(x[i]);
  }
  return out;
}

// intersect -------------------------------------------------------------------

// At most two intervals will be intersecting, so we don't need to worry about
// intersections of 3+ intervals.
NumericMatrix intersect(const Endpoints& endpoints) {
  std::vector<double> starts, ends;
  starts.reserve(endpoints.size() / 2);
  ends.reserve(endpoints.size() / 2);

  int score { 0 };
  for (const Endpoint& endpoint : endpoints) {
    if (endpoint.is_start) {
      ++score;
      // We've entered an intersection
      if (score == 2) {
        starts.push_back(endpoint.value);
      }
    } else {
      --score;
      // We've exited an intersection
      if (score == 1) {
        ends.push_back(endpoint.value);
      }
    }
  }

  int n = starts.size();
  NumericMatrix out(n, 2);
  std::copy(starts.begin(), starts.end(), out.begin());
  std::copy(ends.begin(), ends.end(), out.begin() + n);

  return out;
}

// [[Rcpp::export]]
NumericMatrix intersect_interval_set(NumericMatrix x, NumericMatrix y) {
  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0 || ISNA(x[0])) return x;
  if (ny == 0 || ISNA(y[0])) return y;

  Endpoints endpoints;
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
  return intersect(endpoints);
}

// [[Rcpp::export]]
List v_intersect_interval_set(const List& x, const List& y) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = intersect_interval_set(x[i], y[i]);
  }
  return out;
}

// overlaps --------------------------------------------------------------------

bool overlaps(const Endpoints& endpoints) {
  int score { 0 };
  for (const Endpoint& endpoint : endpoints) {
    if (endpoint.is_start) {
      ++score;
    } else {
      --score;
    }
    if (score > 1) {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
int overlaps_interval_set(NumericMatrix x, NumericMatrix y) {
  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0 || ny == 0) return false;
  if (ISNA(x[0]) || ISNA(y[0])) return NA_LOGICAL;

  Endpoints endpoints;
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
  return overlaps(endpoints);
}

// [[Rcpp::export]]
LogicalVector v_overlaps_interval_set(const List& x, const List& y) {
  int n = x.size();
  LogicalVector out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = overlaps_interval_set(x[i], y[i]);
  }
  return out;
}

// within ----------------------------------------------------------------------

// TODO: We could move these helper functions (e.g. `overlaps()`) within the
//       wrapper function, since we never re-use them.
bool within(const BinaryEndpoints& endpoints) {
  bool in_x_interval { false };
  for (const BinaryEndpoint& endpoint : endpoints) {
    if (endpoint.in_x) {
      // Encountering a start from `x` indicates that we've entered an `x`
      // interval and an end indicates that we've exited an `x` interval.
      in_x_interval = endpoint.is_start;
    } else {
      // Encountering any endpoint from `y` while we're not within an `x`
      // interval indicates that `y` is not within `x`.
      if (!in_x_interval) return false;
    }
  }
  return true;
}

// [[Rcpp::export]]
int within_intervalset(NumericMatrix x, NumericMatrix y) {
  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0 || ny == 0) return false;
  if (ISNA(x[0]) || ISNA(y[0])) return NA_LOGICAL;

  BinaryEndpoints endpoints;
  endpoints.reserve((nx + ny) * 2);

  for (int i { 0 }; i < nx; ++i) {
    endpoints.push_back(BinaryEndpoint { true, true, x[i] });
    endpoints.push_back(BinaryEndpoint { false, true, x[i + nx] });
  }
  for (int i { 0 }; i < ny; ++i) {
    endpoints.push_back(BinaryEndpoint { true, false, y[i] });
    endpoints.push_back(BinaryEndpoint { false, false, y[i + ny] });
  }

  std::sort(endpoints.begin(), endpoints.end());
  return within(endpoints);
}

// [[Rcpp::export]]
LogicalVector v_within_intervalset(const List& x, const List& y) {
  int n = x.size();
  LogicalVector out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = within_intervalset(x[i], y[i]);
  }
  return out;
}

// contains --------------------------------------------------------------------

// [[Rcpp::export]]
int contains_point(NumericMatrix x, double t) {
  int n { x.nrow() };
  if (n == 0) return false;
  if (ISNA(x[0]) || ISNA(t)) return NA_LOGICAL;

  // Anticipating that most interval sets will contain < 3 intervals, so solving
  // these cases immediately.
  switch(n) {
    case 1: return (x[1] <= t && t <= x[2]);
    case 2: return (x[1] <= t && t <= x[2]) || (x[3] <= t && t <= x[4]);
  }

  // Binary search for the nearest interval start strictly less than `t`
  int l { 0 };
  int r { n };
  int m { l + ((r - l) / 2) };
  while(l < r) {
    if (x[m] < t) {
      l = m + 1;
    } else if (x[m] == t) {
      // TODO: We could remove this `else if` condition on each iteration and
      // instead find nearest start less than or equal to `t`. Then we do one
      // check at the end `x[m] <= t && t <= x[m + n]` instead.

      return true; // Inclusive of endpoints (e.g. 1 and [0, 1] overlap)
    } else {
      r = m;
    }
    m = l + ((r - l) / 2);
  }

  // Only check the m-th end as the m-th start is strictly less than `t`
  return t <= x[m + n];
}

// [[Rcpp::export]]
LogicalVector v_contains_point(const List& x, NumericVector t) {
  int n = x.size();
  LogicalVector out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = contains_point(x[i], t[i]);
  }
  return out;
}

// instants --------------------------------------------------------------------

// [[Rcpp::export]]
NumericMatrix remove_instants(NumericMatrix x) {
  int n { x.nrow() };
  if (n == 0 || ISNA(x[0])) return x;

  std::vector<int> keep {};
  keep.reserve(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] != x[i + n]) keep.push_back(i);
  }

  int n_keep = keep.size();
  if (n_keep == n) return x;

  NumericMatrix out(n_keep, 2);
  for (int i { 0 }; i < n_keep; ++i) {
    out[i] = x[keep[i]];
    out[i + n_keep] = x[keep[i] + n];
  }
  return out;
}

// [[Rcpp::export]]
List v_remove_instants(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = remove_instants(x[i]);
  }
  return out;
}
