#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <math.h>

#include "macros.h"

SEXP mof_MOP2(SEXP rx) {
  EXTRACT_NUMERIC_VECTOR(rx, x, n);

  SEXP rf = ALLOC_REAL_VECTOR(n);
  double* f = REAL(rf);

  unsigned int i;
  double a = 0.0, b = 0.0;
  for (i = 0; i < n; ++i) {
    a = a + pow(x[i] - 1 / sqrt(n), 2);
  }
  a = exp(-1 * a);
  for (i = 0; i < n; ++i) {
    b = b + pow(x[i] + 1 / sqrt(n), 2);
  }
  b = exp(-1 * b);

  f[0] = 1 - a;
  f[1] = 1 - b;

  UNPROTECT(1);
  return(rf);
}
