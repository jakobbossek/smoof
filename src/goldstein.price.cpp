#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double goldsteinPriceCPP(const NumericVector x) {
    double a = pow(x[0] + x[1] + 1, 2);
    double b = 19 - 14 * x[0] + 13 * pow(x[0], 2) - 14 * x[1] + 6 * x[0] * x[1] + 3 * pow(x[1], 2);
    double c = pow(2 * x[0] + 3 * x[1], 2);
    double d = 18 - 32 * x[0] + 12 * pow(x[0], 2) + 48 * x[1] - 36 * x[0] * x[1] + 27 * pow(x[1], 2);

    return ((1 + a * b) * (30 + c * d));
}
