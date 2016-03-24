#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <math.h>

#include "macros.h"

SEXP mof_MOP3(SEXP rx) {
  EXTRACT_NUMERIC_VECTOR(rx, x, n);

  SEXP rf = ALLOC_REAL_VECTOR(n);
  double* f = REAL(rf);

  double A1 = 0.5 * sin(1) - 2 * cos(1) + sin(2) - 1.5 * cos(2);
  double A2 = 1.5 * sin(1) - cos(1) + 2 * sin(2) - 0.5 * cos(2);
  double B1 = 0.5 * sin(x[0]) - 2 * cos(x[0]) + sin(x[1]) - 1.5 * cos(x[1]);
  double B2 = 1.5 * sin(x[0]) - cos(x[0]) + 2 * sin(x[1]) - 0.5 * cos(x[1]);

  f[0] = -1 - pow(A1 - B1, 2) - pow(A2 - B2, 2);
  f[1] = -pow(x[0] + 3, 2) - pow(x[1] + 1, 2);

  UNPROTECT(1);
  return(rf);
}
