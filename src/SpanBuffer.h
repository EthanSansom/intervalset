#ifndef INTERVALSET_SPANBUFFER_H
#define INTERVALSET_SPANBUFFER_H

#include <Rcpp.h>
using namespace Rcpp;

class SpanBuffer {
private:
  NumericVector s_starts;
  NumericVector s_ends;
  IntegerVector s_sizes;

  double* p_starts;
  double* p_ends;
  int* p_sizes;

  const int n_elements;
  int current_element = 0;
  int current_point = 0;
  int element_span_count = 0;
public:
  SpanBuffer(int n, int max_spans);
  void add_na_element();
  void add_empty_element();
  void add_scalar_element(double start, double end);
  void add_span(double start, double end);
  void finish_element();
  List get_results();
};

struct SpanSetView {
  const double* starts;
  const double* ends;
  int size;

  bool is_empty() const { return !size; }
};

#endif
