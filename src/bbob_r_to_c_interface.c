// load R headers
#include <R.h>
#include <Rinternals.h>

#include "macros.h"

// The BBOB header file use function with names ERROR
// and WARNING, but these are macros defined in Rcpp
// seemingly. We this 'undefine' this Rcpp stuff here.
// Probably it is not the best idea. We should rather
// rename the BBOB functions.
// FIXME: better rename the functions in the BBOB code
#ifdef ERROR
  #undef ERROR
#endif
#ifdef WARNING
  #undef WARNING
#endif

// import headers of BBOB test set C implementation
#include "bbobStructures.h"
#include "benchmarkshelper.h"
#include "benchmarks.h"
#include "benchmarksdeclare.h"

// some variables for bookkeeping
static int init = 0;

// all the BBOB functions have to be instantiated.
// These varaibles serve to keep track of the lastly initiated function
static unsigned int last_fid;
static unsigned int last_iid;
static unsigned int last_dimension;

// this array of references to the BBOB functions makes if statements needless later
static bbobFunction bbob_funs[24] = {&f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8, &f9, &f10, &f11, &f12, &f13, &f14, &f15, &f16, &f17, &f18, &f19, &f20, &f21, &f22, &f23, &f24};


// This function is responsable for the init process of BBOB functions.
//
// @param dimension [unsigned int] Dimension of the problem.
// @param fid [unsigned int] Function id. Integer in {1, ..., 24}.
// @param iid [unsigned int] Instance id.
//
// @return Nothing. Just do some side-effects.
static void initializeBBOBFunction(const unsigned int dimension, const unsigned int fid, const unsigned int iid) {
  if (init == 0 || last_fid != fid || last_iid != iid || last_dimension != dimension) {
    if (init != 0) {
      finibenchmarks();
      finibenchmarkshelper();
      init = 0;
    }
    // init BBOB function
    isInitDone = 0;
    DIM = dimension;
    last_dimension = dimension;

    // call BBOB initilizer functions
    initbenchmarkshelper();
    initbenchmarks();
    trialid = last_iid = iid;
    last_fid = fid;

    // inititialization finished
    init = 1;
    Fopt = computeFopt(fid, iid);
  }
}

// Evaluate a BBOB function call.
//
// @param r_dimension [unsigned int] Dimension of the problem.
// @param r_fid [unsigned int] Function id. Integer in {1, ..., 24}.
// @param r_iid [unsigned int] Instance id.
// @param r_x [R::numeric] Numeric parameter vector of size 'dimension'.
//
// @return [numeric(1)] Function value of the corresponding BBOB function.
SEXP evaluateBBOBFunctionCPP(SEXP r_dimension, SEXP r_fid, SEXP r_iid, SEXP r_x) {
  unsigned int fid = asInteger(r_fid);
  unsigned int iid = asInteger(r_iid);
  unsigned int dimension = asInteger(r_dimension);

  initializeBBOBFunction(dimension, fid, iid);

  // get the bbob function
  bbobFunction bbob_fun = *bbob_funs[last_fid - 1];

  // unwrap integer
  double *param = REAL(r_x);

  // we allow 'vectorized' input here
  // Note: since C stores 2D arrays as a 1D array with the consecutive rows one after
  // another, there is no straight-forward way to access the columns. We thus
  // require the user to pass the parameters col-wise!
  unsigned int n_values = 1;
  if (isMatrix(r_x)) {
    n_values = ncols(r_x);
  }

  // setup R result object and protect R object in C
  SEXP r_value = ALLOC_REAL_VECTOR(n_values);
  double *value = REAL(r_value);

  for (int i = 0; i < n_values; ++i) {
    value[i] = bbob_fun(param + i * dimension).Fval;
  }

  // unprotect for R
  UNPROTECT(1);

  return (r_value);
}

// Get global optimum and global optimum value of a function.
//
// @param r_dimension [unsigned int] Dimension of the problem.
// @param r_fid [unsigned int] Function id. Integer in {1, ..., 24}.
// @param r_iid [unsigned int] Instance id.
//
// @return [List]
SEXP getOptimumForBBOBFunctionCPP(SEXP r_dimension, SEXP r_fid, SEXP r_iid) {
  // unwrap SEXPs
  unsigned int dimension = asInteger(r_dimension);
  unsigned int fid = asInteger(r_fid);
  unsigned int iid = asInteger(r_iid);

  initializeBBOBFunction(dimension, fid, iid);

  // setup R result vars and protect R objects in C
  SEXP r_param  = ALLOC_REAL_VECTOR(dimension); // numeric vector
  SEXP r_value  = ALLOC_REAL_VECTOR(1); // single numeric value
  SEXP r_result = ALLOC_LIST(2); // list with param and value

  // get the C representation of these
  double *param = REAL(r_param);
  double *value = REAL(r_value);
  value[0] = computeFopt(fid, iid);

  // FIXME: use memset?
  for (int i = 0; i < dimension; ++i) {
    param[i] = 0.0;
  }

  // evaluate the function, so that Xopt is written globally
  evaluateBBOBFunctionCPP(r_dimension, r_fid, r_iid, r_param);
  for (int i = 0; i < dimension; ++i) {
    param[i] = Xopt[i];
  }

  // write param and value to list, which is returned
  SET_VECTOR_ELT(r_result, 0, r_param);
  SET_VECTOR_ELT(r_result, 1, r_value);

  // unprotect for R
  UNPROTECT(3);
  return (r_result);
}
