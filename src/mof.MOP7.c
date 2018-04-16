#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <math.h>

#include "macros.h"

SEXP mof_MOP7(SEXP rx) {
  EXTRACT_NUMERIC_VECTOR(rx, x, n);

  SEXP rf = ALLOC_REAL_VECTOR(3);
  double* f = REAL(rf);

  f[0] = pow(x[0] - 2, 2) / 2.0 + pow(x[1] + 1, 2) / 13.0 + 3;
  f[1] = pow(x[0] + x[1] - 3, 2) / 36.0 + pow(-x[0] + x[1] + 2, 2) / 8.0 - 17;
  f[2] = pow(x[0] + 2 * x[1] - 1, 2) / 175.0 + pow(-x[0] + 2 * x[2], 2) / 17.0 - 13;

  UNPROTECT(1);
  return(rf);
}
