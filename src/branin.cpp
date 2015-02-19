#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double braninCPP(const NumericVector x) {
    double s1 = pow(x[1] - (5 * pow(x[0], 2)) / (4 * pow(3.1415, 2) + (5 * x[0]) / 3.1415) - 6, 2);
    double s2 = 10 * (1 - 1 / (8 * 3.1415)) * cos(x[0]);
    return s1 + s2 + 10;
}
