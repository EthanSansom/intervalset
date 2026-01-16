#ifndef INTERVALSET_V2_SPANSETBUFFER_H
#define INTERVALSET_V2_SPANSETBUFFER_H

#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

class SpanSetBuffer {
private:
  IntegerVector size;
  List starts;
  List ends;
  int* p_size;
  R_xlen_t current_elt = 0;

  std::vector<double> temp_starts;
  std::vector<double> temp_ends;

public:
  SpanSetBuffer(R_xlen_t n, int reserve_size = 8);
  void add_na_element();
  void add_empty_element();
  void add_scalar_element(double start, double end);
  void add_span(double start, double end);
  void finish_element();
  List get_results();
};

#endif
