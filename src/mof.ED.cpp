# include <RcppArmadillo.h>
#include <math.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mof_ED_1(NumericVector x, int m, double gamma, NumericVector theta) {
  int i;
  int j;
  NumericVector f(m);
  NumericVector p(m);

  int n = x.size();

  // define p_1(theta[1], ..., theta[m-1])
  p(0) = pow(cos(theta(0)), 2/gamma);
  // define p_i(theta[1], ..., theta[m-1]), 1 < i < m (i.e., i = 2, ..., m - 1)
  if (m > 2) {
    for (i = 1; i < m - 1; i++) {
      p(i) = 1.0;
      for (j = 0; j < i; j++) {
        p(i) = p(i) * sin(theta(j));
      }
      p(i) = p(i) * cos(theta(i));
      p(i) = pow(p(i), 2 / gamma);
    }
  }
  // define p_m(theta[1], ..., theta[m-1])
  p(m - 1) = 1.0;
  for (j = 0; j < m - 1; j++) {
    p(m - 1) = sin(theta(j));
  }
  p(m - 1) = pow(p(m - 1), 2 / gamma);

  // compute r
  double r = 0.0;
  for (i = m - 1; i < n; i++) {
    r += pow(x(i), 2.0);
  }
  r = pow(r, 0.5);

  // finally, compute f
  for (i = 0; i < m; i++) {
    f(i) = (1 / (r + 1)) * p(i);
  }

  return f;
}



// [[Rcpp::export]]
NumericVector mof_ED_2(NumericVector x, int m, double gamma, NumericVector theta) {
  int i;
  int j;
  NumericVector f(m);
  NumericVector p(m);
  
  int n = x.size();
  
  // define p_1(theta[1], ..., theta[m-1])
  p(0) = pow(cos(theta(0)), 2/gamma);
  // define p_i(theta[1], ..., theta[m-1]), 1 < i < m (i.e., i = 2, ..., m - 1)
  if (m > 2) {
    for (i = 1; i < m - 1; i++) {
      p(i) = 1.0;
      for (j = 0; j < i; j++) {
        p(i) = p(i) * sin(theta(j));
      }
      p(i) = p(i) * cos(theta(i));
      p(i) = pow(p(i), 2 / gamma);
    }
  }
  // define p_m(theta[1], ..., theta[m-1])
  p(m - 1) = 1.0;
  for (j = 0; j < m - 1; j++) {
    p(m - 1) = sin(theta(j));
  }
  p(m - 1) = pow(p(m - 1), 2 / gamma);
  
  // compute r
  double r = 0.0;
  for (i = m - 1; i < n; i++) {
    r += pow(x(i), 2.0);
  }
  r = pow(r, 0.5);
  
  // until here, this was identical to ED1, but now we need to transform r
  double a = 0.051373;
  double b = 0.051373;
  double Fnatmin = b + (r - a) + 0.5 + 0.5 * cos(2 * M_PI * (r - a) + M_PI);
  
  // finally, compute f
  for (i = 0; i < m; i++) {
    f(i) = (1 / (Fnatmin + 1)) * p(i);
  }
  
  return f;
}
