#ifndef INTERVALSET_V0_SET_OPERATIONS_H
#define INTERVALSET_V0_SET_OPERATIONS_H

#include <Rcpp.h>
using namespace Rcpp;

List cpp_intersect_interval_sets(const List& x, const List& y);
NumericMatrix intersect_interval_set(NumericMatrix x, NumericMatrix y);

#endif
