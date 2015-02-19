#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double doubleSumCPP(const NumericVector x) {
    double sum = 0.0;
    int n = x.size();
    for (int i = 0; i < n; ++i) {
        double inner_sum = 0.0;
        for (int j = 0; j <= i; ++j) {
            inner_sum += x[j];
        }
        sum += pow(inner_sum, 2);
    }
    return sum;
}
