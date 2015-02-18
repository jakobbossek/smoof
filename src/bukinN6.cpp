#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double bukinN6CPP(const NumericVector x) {
    return (100 * sqrt(abs(x[1] - 0.01 * pow(x[0], 2))) + 0.01 * abs(x[0] + 10));
}
