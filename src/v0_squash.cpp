#include "v0_endpoint.h"
#include "v0_utils.h"
#include <Rcpp.h>
using namespace Rcpp;

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

  return new_matrix(starts, ends);
}

// [[Rcpp::export]]
NumericMatrix squash_v0_cpp(const NumericMatrix x) {
  int n { x.nrow() };
  if (n <= 1) return x;

  Endpoints endpoints;
  endpoints.reserve(n * 2);

  for (int i { 0 }; i < n; ++i) {
    endpoints.push_back(Endpoint { true, x[i] });
    endpoints.push_back(Endpoint { false, x[i + n] });
  }

  std::sort(endpoints.begin(), endpoints.end());
  return squash(endpoints);
}
