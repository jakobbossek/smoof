#include <Rcpp.h>

// FIXME: better rename the functions in the BBOB code
// these are later redefined
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

using namespace Rcpp;

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

  initbenchmarkshelper();
  initbenchmarks();
  trialid = last_iid = iid;
  last_fid = fid;
  // inititialization done
  init = 1;
}

// [[Rcpp::export]]
double evaluateBBOBFunctionCPP(const unsigned int dimension, const unsigned int fid, const unsigned int iid, NumericVector x) {
  initializeBBOBFunction(dimension, fid, iid);

  // FIXME: set up std::map from integer to BBOB function. Can we 'handles'?
  double* param = new double[dimension];
  for (int i = 0; i < dimension; ++i) {
    param[i] = x[i];
  }
  switch(last_fid) {
    case 1: return f1(param).Fval;
    default: return 0.000001;
  }
}

// [[Rcpp::export]]
List getOptimumForBBOBFunctionCPP(const unsigned int dimension, const unsigned int fid, const unsigned int iid) {
  initializeBBOBFunction(dimension, fid, iid);

  double value = computeFopt(fid, iid);
  // every function has its minimum in the origin (0, ..., 0)
  // We hence initialize a numeric vector of size 'dimension'
  NumericVector x(dimension);

  // evaluate the function at that point ...
  evaluateBBOBFunctionCPP(fid, iid, dimension, x);

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
