#ifndef INTERVALSET_ENDPOINT_H_
#define INTERVALSET_ENDPOINT_H_

#include <vector>

struct Endpoint {
  bool is_start {};
  double value {};
};

using Endpoints = std::vector<Endpoint>;

bool operator< (const Endpoint &e1, const Endpoint &e2) {
  if (e1.value == e2.value) {
    return e1.is_start < e2.is_start;
  }
  return e1.value < e2.value;
}

#endif
