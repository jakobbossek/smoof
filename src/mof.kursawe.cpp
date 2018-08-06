#include <RcppArmadillo.h>
#include <math.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector kursawe(arma::vec x) {

  NumericVector f(2);
  int n = x.size();

  double f1 = 0.0;
  double f2 = 0.0;
  double innerSum = 0.0;

  for (int i = 0; i < n - 1; i++) {
    innerSum = pow(x[i], 2.0) + pow(x[i+1], 2.0);
    f1 += -10 * exp(-0.2 * pow(innerSum, 0.5));
    f2 += pow(abs(x[i]), 0.8) + 5 * pow(sin(x[i]), 3.0);
  }
  f2 += pow(abs(x[n-1]), 0.8) + 5 * pow(sin(x[n-1]), 3.0);

  f(0) = f1;
  f(1) = f2;
  return f;
}
