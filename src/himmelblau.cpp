#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double himmelblauCPP(const NumericVector x) {
    return (pow(pow(x[0], 2) + x[1] - 11, 2) + pow(x[0] + pow(x[1], 2) - 7, 2));
}
