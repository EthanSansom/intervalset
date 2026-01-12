#include "SpanBuffer.h"

#include <Rcpp.h>
using namespace Rcpp;

SpanBuffer::SpanBuffer(int n, int max_spans) : n_elements(n) {
  s_starts = NumericVector(no_init(max_spans));
  s_ends = NumericVector(no_init(max_spans));
  s_sizes = IntegerVector(no_init(n));

  p_starts = s_starts.begin();
  p_ends = s_ends.begin();
  p_sizes = s_sizes.begin();
}

void SpanBuffer::add_na_element() {
  p_starts[current_point] = NA_REAL;
  p_ends[current_point] = NA_REAL;
  current_point++;

  p_sizes[current_element] = NA_INTEGER;
  current_element++;
  element_span_count = 0;
}

void SpanBuffer::add_empty_element() {
  p_sizes[current_element] = 0;
  current_element++;
  element_span_count = 0;
}

void SpanBuffer::add_scalar_element(double start, double end) {
  p_starts[current_point] = start;
  p_ends[current_point] = end;
  current_point++;

  p_sizes[current_element] = 1;
  current_element++;
  element_span_count = 0;
}

void SpanBuffer::add_span(double start, double end) {
  p_starts[current_point] = start;
  p_ends[current_point] = end;
  current_point++;
  element_span_count++;
}

void SpanBuffer::finish_element() {
  p_sizes[current_element] = element_span_count;
  current_element++;
  element_span_count = 0;
}

List SpanBuffer::get_results() {
  return List::create(
    // Named("starts") = s_starts[Range(0, current_point - 1)],
    // Named("ends") = s_ends[Range(0, current_point - 1)],

    Named("starts") = head(s_starts, current_point),
    Named("ends") = head(s_ends, current_point),
    Named("sizes") = s_sizes
  );
}
