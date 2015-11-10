// [[Rcpp::depends(RcppArmadillo)]]
# include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP dtlz_1(arma::vec x, int M) {
  int i;
  arma::vec f(M);
  
  int n = x.size();
  int k = n - M + 1;
  arma::vec xm = x.subvec(M-1, n-1);
  
  double g = 100 * (k + sum(arma::pow((xm - 0.5), 2) - arma::cos(20 * M_PI * (xm - 0.5))));
  
  f.fill(0.5 * (1 + g));
  
  double prod_xi = 1.0;
  
  for(i=M-1; i > 0; i--) {
    f(i) *= prod_xi * (1 - x(M - (i + 1)));
    prod_xi = prod_xi * x(M - (i + 1));
  }
  f(0) *= prod_xi;
  return NumericVector(f.begin(), f.end());
}

// [[Rcpp::export]]
SEXP dtlz_2(arma::vec x, int M) {
  int i;
  arma::vec f(M);
  
  int n = x.size();
  arma::vec xm = x.subvec(M-1, n-1);
  
  double g = sum(arma::pow((xm - 0.5), 2));
  
  f.fill(1 + g);
  
  double prod_xi = 1.0;
  
  for(i=M-1; i > 0; i--) {
    f(i) *= prod_xi * sin(x(M - (i + 1)) * M_PI * 0.5);
    prod_xi = prod_xi * cos(x(M - (i + 1)) * M_PI * 0.5);
  }
  f(0) *= prod_xi;
  return NumericVector(f.begin(), f.end());
}

// [[Rcpp::export]]
SEXP dtlz_3(arma::vec x, int M) {
  int i;
  arma::vec f(M);
  
  int n = x.size();
  int k = n - M + 1;
  arma::vec xm = x.subvec(M-1, n-1);
  
  double g = 100 * (k + sum(arma::pow((xm - 0.5), 2) - arma::cos(20 * M_PI * (xm - 0.5))));
  
  f.fill(1 + g);
  
  double prod_xi = 1.0;
  
  for(i=M-1; i > 0; i--) {
    f(i) *= prod_xi * sin(x(M - (i + 1)) * M_PI * 0.5);
    prod_xi = prod_xi * cos(x(M - (i + 1)) * M_PI * 0.5);
  }
  f(0) *= prod_xi;
  return NumericVector(f.begin(), f.end());
}

// [[Rcpp::export]]
SEXP dtlz_4(arma::vec x, int M, double alpha=100) {
  int i;
  arma::vec f(M);
  
  int n = x.size();
  arma::vec xm = x.subvec(M-1, n-1);
  
  double g = sum(arma::pow((xm - 0.5), 2));
  
  x = pow(x, alpha);
  
  f.fill(1 + g);
  
  double prod_xi = 1.0;
  
  for(i=M-1; i > 0; i--) {
    f(i) *= prod_xi * sin(x(M - (i + 1)) * M_PI * 0.5);
    prod_xi = prod_xi * cos(x(M - (i + 1)) * M_PI * 0.5);
  }
  f(0) *= prod_xi;
  return NumericVector(f.begin(), f.end());
}

// [[Rcpp::export]]
SEXP dtlz_5(arma::vec x, int M) {
  int i;
  arma::vec f(M), theta(M-1);
  
  int n = x.size();
  arma::vec xm = x.subvec(M-1, n-1);
  
  double g = sum(arma::pow((xm - 0.5), 2));
  
  double t = M_PI / (4 * (1 + g));
  
  theta(0) = x(0) * M_PI / 2;
  if(M-2 > 0)
    theta.subvec(1, M-2) = t * (1 + 2 * g * x.subvec(1, M-2));
  
  f.fill(1+g);
  
  double prod_xi = 1.0;
  
  for(i=M-1; i > 0; i--) {
    f(i) *= prod_xi * sin(theta(M - (i + 1)));
    prod_xi = prod_xi * cos(theta(M - (i + 1)));
  }
  f(0) *= prod_xi;
  return NumericVector(f.begin(), f.end());
}

// [[Rcpp::export]]
SEXP dtlz_6(arma::vec x, int M) {
  int i;
  arma::vec f(M), theta(M-1);
  
  int n = x.size();
  int k = n - M + 1;
  
  arma::vec g_vec = arma::pow(x.subvec(n - k, n - 1), 0.1);
  
  double g = sum(g_vec);
  
  double t = M_PI / (4 * (1 + g));
  
  theta(0) = x(0) * M_PI / 2;
  if(M-2 > 0)
    theta.subvec(1, M-2) = t * (1 + 2 * g * x.subvec(1, M-2));
  
  f.fill(1+g);
  
  double prod_xi = 1.0;
  
  for(i=M-1; i > 0; i--) {
    f(i) *= prod_xi * sin(theta(M - (i + 1)));
    prod_xi = prod_xi * cos(theta(M - (i + 1)));
  }
  f(0) *= prod_xi;
  return NumericVector(f.begin(), f.end());
}

// [[Rcpp::export]]
SEXP dtlz_7(arma::vec x, int M) {
  arma::vec f(M), theta(M-1);
  
  int n = x.size();
  int k = n - M + 1;
  arma::vec xm = x.subvec(M-1, n-1);
  
  double g = 1 + (9 * sum(xm)) / k;
  
  f.subvec(0, M-2) = x.subvec(0, M-2);
  
  arma::vec fi = f.subvec(0, M-2);
  
  double h = M - sum(fi % (1 + arma::sin(3 * M_PI * fi)) / (1 + g));
  f(M-1) = (1 + g) * h;
  
  return NumericVector(f.begin(), f.end());
}
