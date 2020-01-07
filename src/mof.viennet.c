#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <math.h>

#include "macros.h"

SEXP mof_viennet(SEXP r_x) {
  EXTRACT_NUMERIC_VECTOR(r_x, c_x, c_dim);

  SEXP r_res = ALLOC_REAL_VECTOR(3);
  double* c_res = REAL(r_res);

  double ss = c_x[0] * c_x[0] + c_x[1] * c_x[1];
  c_res[0] = 0.5 * ss + sin(ss);
  c_res[1] = pow(3 * c_x[0] - 2 * c_x[1] + 4, 2) / 8.0 + pow(c_x[0] - c_x[1] + 1, 2) / 27.0 + 15;
  c_res[2] = 1.0 / (ss + 1) - 1.1 * exp(-ss);

  UNPROTECT(1);
  return(r_res);
}
