#include "endpoint.h"
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// These functions receive a numeric vector of starts and of ends coming from a
// lubridate::interval(). Ends may be less than starts. We directly convert
// these starts and ends into a NumericMatrix or List of NumericMatrix suitable
// for conversion to an interval set.

// TODO:
// - List as_phinterval(starts, ends) -> convert starts and ends into list of numeric matrices

// NOTE:
// - Maybe use S4 methods so we can do multiple dispatch on (interval, interval)
//   (phinterval, interval), (phinterval, phinterval).
