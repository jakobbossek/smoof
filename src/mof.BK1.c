#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <math.h>

#include "macros.h"

SEXP mof_bk1(SEXP r_x) {
  EXTRACT_NUMERIC_VECTOR(r_x, c_x, c_dim);

  SEXP r_res = ALLOC_REAL_VECTOR(c_dim);
  double* c_res = REAL(r_res);

  c_res[0] = c_x[0] + c_x[1];
  c_res[1] = pow(c_x[0] - 5, 2) + pow(c_x[1] - 5, 2);

  UNPROTECT(1);
  return(r_res);
}
