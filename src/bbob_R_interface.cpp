#include <Rcpp.h>

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
extern "C" {
  #include "bbobStructures.h"
  #include "benchmarkshelper.h"
  #include "benchmarks.h"
  //#include "benchmarksnoisy.h"
  #include "benchmarksdeclare.h"
}

// some variables for bookkeeping
static int init = 0;
static unsigned int last_fid;
static unsigned int last_iid;
static unsigned int last_dimension;
static bbobFunction bbob_funs[24] = {&f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8, &f9, &f10, &f11, &f12, &f13, &f14, &f15, &f16, &f17, &f18, &f19, &f20, &f21, &f22, &f23, &f24};


using namespace Rcpp;

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
double evaluateBBOBFunctionCPP(const unsigned int dimension, const unsigned int fid, const unsigned int iid, NumericVector x) {
  initializeBBOBFunction(dimension, fid, iid);

  // FIXME: set up std::map from integer to BBOB function. Can we 'handles'?
  double* param = new double[dimension];
  for (int i = 0; i < dimension; ++i) {
    param[i] = x[i];
  }
  if (last_fid > 24) {
    return 0.000001;
  }

  bbobFunction bbob_fun = *bbob_funs[last_fid - 1];
  return(bbob_fun(param).Fval);
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
List getOptimumForBBOBFunctionCPP(const unsigned int dimension, const unsigned int fid, const unsigned int iid) {
  initializeBBOBFunction(dimension, fid, iid);

  // get the optimal value
  double value = computeFopt(fid, iid);

  //initializeBBOBFunction(dimension, fid, iid);

  NumericVector x(dimension);
  for (int i = 0; i < x.size(); ++i) {
    x[i] = 0.0;
  }
  initializeBBOBFunction(dimension, fid, iid);

  // evaluate the function at that point ...
  evaluateBBOBFunctionCPP(dimension, fid, iid, x);

  // and store the result in a numeric vector for output
  NumericVector param(dimension);
  for (int i = 0; i < dimension; ++i) {
    param[i] = Xopt[i];
  }

  return List::create(
    _["param"] = param,
    _["value"] = NumericVector::create(value)
  );
}
