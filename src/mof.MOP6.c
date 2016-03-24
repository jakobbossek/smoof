#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <math.h>

#include "macros.h"

SEXP mof_MOP6(SEXP rx) {
  EXTRACT_NUMERIC_VECTOR(rx, x, n);

  SEXP rf = ALLOC_REAL_VECTOR(n);
  double* f = REAL(rf);

  f[0] = x[0];
  double a = (1 + 10 * x[1]);
  double b = x[0] / a;
  f[1] = a * (1 - pow(b, 2) - b * sin(8 * M_PI * x[0]));

  UNPROTECT(1);
  return(rf);
}
