#include "endpoint.h"
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

void squash(
    const Endpoints& endpoints,
    std::vector<double>& out_starts,
    std::vector<double>& out_ends
) {
  int score { 0 };
  for (Endpoint endpoint : endpoints) {
    if (endpoint.is_start) {
      // We're at the first start of a union
      if (score == 0) {
        out_starts.push_back(endpoint.value);
      }
      ++score;
    } else {
      // We're at the last end of a union
      if (score == 1) {
        out_ends.push_back(endpoint.value);
      }
      --score;
    }
  }
}

// [[Rcpp::export]]
NumericMatrix squash_intervals(NumericMatrix x) {
  int nx { x.nrow() };

  if (nx <= 1) return x;

  // TODO: Make into function? Could do `push_endpoints()` to push the matrix
  //      points into the list.
  std::vector<Endpoint> endpoints;
  endpoints.reserve(nx * 2);

  for (int i { 0 }; i < nx; ++i) {
    endpoints.push_back(Endpoint { true, x[i] });
    endpoints.push_back(Endpoint { false, x[i + nx] });
  }

  // TODO: Yeah, all of this is super repetitive (albeit just twice), may want
  //       to convert into a function.
  std::vector<double> starts, ends;
  starts.reserve(nx);
  ends.reserve(nx);

  std::sort(endpoints.begin(), endpoints.end());
  squash(endpoints, starts, ends);

  int n = starts.size();
  NumericMatrix result(n, 2);
  std::copy(starts.begin(), starts.end(), result.begin());
  std::copy(ends.begin(), ends.end(), result.begin() + n);

  return result;
}

// [[Rcpp::export]]
NumericMatrix merge_intervals(NumericMatrix x, NumericMatrix y) {
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

  // At most we'll have nx + ny intervals (if x, y are disjoint)
  // This might not help at all, do some benchmarks
  std::vector<double> starts, ends;
  starts.reserve(nx + ny);
  ends.reserve(nx + ny);

  std::sort(endpoints.begin(), endpoints.end());
  squash(endpoints, starts, ends);

  int n = starts.size();
  NumericMatrix result(n, 2);
  std::copy(starts.begin(), starts.end(), result.begin());
  std::copy(ends.begin(), ends.end(), result.begin() + n);

  return result;
}
