#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <math.h>

#include "macros.h"

SEXP mof_MOP1(SEXP rx) {
  EXTRACT_NUMERIC_VECTOR(rx, x, n);

  SEXP rf = ALLOC_REAL_VECTOR(2);
  double* f = REAL(rf);

  f[0] = pow(x[0], 2);
  f[1] = pow(x[0] - 2.0, 2);

  UNPROTECT(1);
  return(rf);
}
