#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <math.h>

#include "macros.h"

SEXP mof_MOP4(SEXP rx) {
  EXTRACT_NUMERIC_VECTOR(rx, x, n);

  SEXP rf = ALLOC_REAL_VECTOR(n);
  double* f = REAL(rf);

  double f1 = 0.0, f2 = 0.0;
  for (unsigned int i = 0; i < n - 1; ++i) {
    f1 += -10 * exp(-0.2 * sqrt(pow(x[i], 2) + pow(x[i + 1], 2)));
  }

  for (unsigned int i = 0; i < n; ++i) {
    f2 += pow(fabs(x[i]), 0.8) + 5 * sin(pow(x[i], 3));
  }

  f[0] = f1;
  f[1] = f2;

  UNPROTECT(1);
  return(rf);
}
