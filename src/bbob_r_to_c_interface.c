// load R headers
#include <R.h>
#include <Rinternals.h>

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
static unsigned int last_fid;
static unsigned int last_iid;
static unsigned int last_dimension;
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

// Evaluate a BBOB function call.
//
// @param dimension [unsigned int] Dimension of the problem.
// @param fid [unsigned int] Function id. Integer in {1, ..., 24}.
// @param iid [unsigned int] Instance id.
// @param x [Rcpp::NumericVector] Numeric parameter vector of size 'dimension'.
//
// @return [double] Function value of the corresponding BBOB function.
// [[Rcpp::export]]
SEXP evaluateBBOBFunctionCPP(SEXP r_dimension, SEXP r_fid, SEXP r_iid, SEXP r_x) {
  unsigned int fid = asInteger(r_fid);
  unsigned int iid = asInteger(r_iid);
  unsigned int dimension = asInteger(r_dimension);

  //initializeBBOBFunction(dimension, fid, iid);
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

  // generate object for output
  SEXP r_value = PROTECT(allocVector(REALSXP, 1));

  // get the bbob function
  bbobFunction bbob_fun = *bbob_funs[last_fid - 1];

  // unwrap integer
  if (!isReal(r_x)) {
    error("r_x must be numeric!");
  }
  double *param = REAL(r_x);

  double *value = REAL(r_value);
  value[0] = bbob_fun(param).Fval;
  UNPROTECT(1);
  return (r_value);
}

// Get global optimum and global optimum value of a function.
//
// @param dimension [unsigned int] Dimension of the problem.
// @param fid [unsigned int] Function id. Integer in {1, ..., 24}.
// @param iid [unsigned int] Instance id.
// @param x [Rcpp::NumericVector] Numeric parameter vector of size 'dimension'.
//
// @return [Rcpp::List]
// [[Rcpp::export]]
SEXP getOptimumForBBOBFunctionCPP(SEXP r_dimension, SEXP r_fid, SEXP r_iid) {
  // unwrap SEXPs
  unsigned int dimension = INTEGER(r_dimension)[0];
  unsigned int fid = INTEGER(r_fid)[0];
  unsigned int iid = INTEGER(r_iid)[0];

  initializeBBOBFunction(dimension, fid, iid);

  // setup result vars and protect from gc
  SEXP r_param = PROTECT(allocVector(REALSXP, dimension)); // numeric vector
  SEXP r_value = PROTECT(allocVector(REALSXP, 1)); // single numeric value
  SEXP r_result = PROTECT(allocVector(VECSXP, 2)); // list with param and value

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

  UNPROTECT(3);
  return (r_result);
}
