/*
 * R to C interface for the CEC2009-MOEA test functions UF1, ..., UF10.
 *
 * @author Jakob Bossek <j.bossek@gmail.com>
 *
 * The function implemenations is based on the C++ implementation of Qingfu Zhang,
 * Aimin Zhou, Shizheng Zhaoy, Ponnuthurai Nagaratnam Suganthany, Wudong Liu and Santosh Tiwar.
 *
*/
#include <R.h>
#include <Rinternals.h>

#include "macros.h"
#include "cec09.h"

// this array of references to the UF functions makes if statements needless later
void (*UF_funs[10]) (double *x, double *f, const unsigned int nx) = {UF1, UF2, UF3, UF4, UF5, UF6, UF7, UF8, UF9, UF10};

// Evaluate a UFx function call.
//
// @param r_id [unsigned int] ID of the problem (in {1, 2, ..., 10}.
// @param r_x [R::numeric] Numeric parameter vector of size 'dimension'.
// @param r_dimension [unsigned int] Problem dimension > 2.
//
// @return [numeric] Vector of function values.
SEXP evaluateUFFunction(SEXP r_id, SEXP r_x, SEXP r_dimension) {
  if (!isReal(r_x) || !isVector(r_x)) {
    error("You need to pass a numeric vector to the UF functions.");
  }

  unsigned int dimension = asInteger(r_dimension);
  unsigned int id = asInteger(r_id);

  // get C representation of r_x vector
  double *param = REAL(r_x);

  // determine number of objectives
  unsigned int n_objectives = 2;
  if (id > 7) {
    n_objectives = 3;
  }

  // allocate memory for result object
  SEXP r_value = ALLOC_REAL_VECTOR(n_objectives);
  double *value = REAL(r_value);

  // init object
  for (int i = 0; i < n_objectives; ++i) {
    value[i] = 0.0;
  }

  // call UFx function (works on references)
  (*UF_funs[id - 1])(param, value, dimension);

  UNPROTECT(1);
  return(r_value);
}
