#include <RcppArmadillo.h>
#include <math.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector evaluate_nk_landscape(Rcpp::List values, Rcpp::List links, IntegerVector x) {

  NumericVector f(1);
  int N = x.size();

  // storage for function value NK(x)
  double fval = 0.0;

  // for all bit positions ...
  for (unsigned int n = 0; n < N; ++n) {
    IntegerVector links_n = as<IntegerVector>(links[n]);
    unsigned int K = links_n.size();

    // take into consideration the n-th bit-position itself
    unsigned int offset = int(x[n]) * pow(2, K); //

    // ... and for all epistatic links ...
    for (int k = 0; k < K; ++k) {
      // ... build the offset for the values table
      int power2 = 1 << (K - k - 1);
      //int power2 = pow(2, K - k - 1);
      offset += int(x[links_n[k]]) * power2;
    }
    // workaround
    // offset = pow(2, K + 1) - offset;

    fval += as<NumericVector>(values[n])[offset];
  }

  // normalise
  fval /= N;

  // prepare for R
  f(0) = fval;
  return f;
}
