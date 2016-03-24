#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <math.h>

#include "macros.h"

SEXP mof_MOP5(SEXP rx) {
  EXTRACT_NUMERIC_VECTOR(rx, x, n);

  SEXP rf = ALLOC_REAL_VECTOR(n);
  double* f = REAL(rf);

  double x0sq = pow(x[0], 2);
  double x1sq = pow(x[1], 2);

  f[0] = 0.5 * (x0sq + x1sq) + sin(x0sq + x1sq);
  f[1] = pow(3 * x[0] - 2 * x[1] + 4, 2) / 8.0 + pow(x[0] - x[1] + 1, 2) / 27 + 15;
  f[2] = 1 / (x0sq + x1sq + 1) - 1.1 * exp(-x0sq - x1sq);

  UNPROTECT(1);
  return(rf);
}
