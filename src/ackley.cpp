#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double ackleyCPP(const NumericVector x) {
    double a = 20;
    double b = 0.2;
    double c = 2 * 3.1415;
    double n = x.size();

    double sum1 = 0.0, sum2 = 0.0;
    for (int i = 0; i < n; ++i) {
        sum1 += pow(x[i], 2.0);
        sum2 += cos(c * x[i]);
    }

    return (-a * exp(-b * sqrt((1 / n) * sum1)) - exp((1 / n) * sum2) + a + exp(1));
}
