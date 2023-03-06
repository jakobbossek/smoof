#include <RcppArmadillo.h>
#include <math.h>
#include <algorithm>

using namespace Rcpp;

// extract the sign of a number x
double MYSIGN(double x) {
  double res;
  if (x > 0) {
    res = 1.0;
  } else if (x < 0) {
    res = -1.0;
  } else {
    res = 0.0;
  }
  return res;
}

// pick the minimum of two numbers a and b
double MYMIN(double a, double b) {
  double res;
  if (a < b) {
    res = a;
  } else {
    res = b;
  }
  return(res);
}


// [[Rcpp::export]]
NumericVector mof_cec2019_SYM_PART_SIMPLE(NumericVector x, double a, double b, double c) {
  NumericVector y(2);
  double t1hat = MYSIGN(x(0)) * ceil((abs(x(0)) - (a + c / 2)) / (2 * a + c));
  double t2hat = MYSIGN(x(1)) * ceil((abs(x(1)) - (b / 2)) / b);
  double t1 = MYSIGN(t1hat) * MYMIN(abs(t1hat), 1);
  double t2 = MYSIGN(t2hat) * MYMIN(abs(t2hat), 1);
  double p1 = x(0) - t1 * (c + 2 * a);
  double p2 = x(1) - t2 * b;
  y(0) = pow(p1 + a, 2) + pow(p2, 2);
  y(1) = pow(p1 - a, 2) + pow(p2, 2);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_SYM_PART_ROTATED(NumericVector x, double w, double a, double b, double c) {
  NumericVector y(2);
  double r1 = cos(w) * x(0) - sin(w) * x(1);
  double r2 = sin(w) * x(0) + cos(w) * x(1);
  x(0) = r1;
  x(1) = r2;
  y = mof_cec2019_SYM_PART_SIMPLE(x, a, b, c);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_OMNI_Test(NumericVector x) {
  NumericVector y(2);
  int n = x.size();
  for (int i = 0; i < n; i++) {
    y(0) = y(0) + sin(M_PI * x(i));
    y(1) = y(1) + cos(M_PI * x(i));
  }
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf1(NumericVector x) {
  NumericVector y(2);
  y(0) = abs(x(0) - 2);
  y(1) = 1 - sqrt(y(0)) + 2 * pow(x(1) - sin(6 * M_PI * y(0) + M_PI), 2);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf1_z(NumericVector x, double k) {
  // in the CEC2019 competition k = 3 was used
  NumericVector y(2);
  y(0) = abs(x(0) - 2);
  if (x(0) < 2) {
    y(1) = 1 - sqrt(y(0)) + 2 * pow(x(1) - sin(2 * k * M_PI * y(0) + M_PI), 2);
  } else {
    y(1) = 1 - sqrt(y(0)) + 2 * pow(x(1) - sin(2 * M_PI * y(0) + M_PI), 2);
  }
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf1_e(NumericVector x, double a) {
  // FIXME
  // in the CEC2019 competition a = e was used
  NumericVector y(2);
  y(0) = abs(x(0) - 2);
  if (x(0) < 2) {
    y(1) = 1 - sqrt(y(0)) + 2 * pow(x(1) - sin(6 * M_PI * y(0) + M_PI), 2);
  } else {
    y(1) = 1 - sqrt(y(0)) + 2 * pow(x(1) - pow(a, x(0)) * sin(6 * M_PI * y(0) + M_PI), 2);
  }
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf2(NumericVector x) {
  // FIXME
  NumericVector y(2);
  y(0) = x(0);
  if (x(1) > 1) {
    x(1) = x(1) - 1;
  }
  double y2 = x(1) - sqrt(x(0));
  y(1) = 1 - sqrt(x(0)) + 2 * (4 * pow(y2, 2) - 2 * cos(20 * y2 * M_PI / sqrt(2)) + 2);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf3(NumericVector x) {
  // FIXME
  NumericVector y(2);
  y(0) = x(0);
  double y2 = x(1) - sqrt(x(0));
  if (x(1) >= 1.0) {
    y2 = y2 - 0.5;
  } else if (x(0) < 0.25) {
    if ((x(1) > 0.5) && (x(1) < 1.0)) {
      y2 = y2 - 0.5;
    }
  }
  y(1) = 1 - sqrt(x(0)) + 2 * (4 * pow(y2, 2) - 2 * cos(20 * y2 * M_PI / sqrt(2)) + 2);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf4(NumericVector x) {
  NumericVector y(2);
  y(0) = x(0);
  if (x(1) >= 1) {
    x(1) = x(1) - 1;
  }
  y(0) = abs(x(0));  
  y(1) = 1.0 - pow(x(0), 2) + 2 * pow(x(1) - sin(M_PI * abs(x(0))), 2);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf5(NumericVector x) {
  NumericVector y(2);
  
  y(0) = abs(x(0) - 2);
  if (x(1) > 1) {
    x(1) = x(1) - 2;
  }
  y(1) = 1 - sqrt(y(0)) + 2 * pow(x(1) - sin(6 * M_PI * y(0) + M_PI), 2);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf6(NumericVector x) {
  // FIXME
  NumericVector y(2);
  
  y(0) = abs(x(0) - 2);
  if (x(1) > 1) {
    x(1) = x(1) - 1;
  }
  y(1) = 1 - sqrt(y(0)) + 2 * pow(x(1) - sin(6 * M_PI * y(0) + M_PI), 2);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf7(NumericVector x) {
  NumericVector y(2);
  double f1 = abs(x(0) - 2);
  double co = 24 * M_PI * f1 + 4 * M_PI;
  double inner = x(1) - (0.3 * pow(f1, 2) * cos(co) + 0.6 * f1) * sin(6 * M_PI * f1 + M_PI);
  y(0) = f1;
  y(1) = 1.0 - sqrt(f1) + pow(inner, 2);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf8(NumericVector x) {
  NumericVector y(2);
  double a = abs(x(0));
  double b = sin(a);
  y(0) = b;
  if (x(1) > 4) {
    x(1) = x(1) - 4;
  }
  y(1) = sqrt(1 - pow(b, 2)) + 2 * pow(x(1) - b - a, 2);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf9(NumericVector x, int np) {
  NumericVector y(2);
  y(0) = x(0);
  double g = 2 - pow(sin(np * M_PI * x(1)), 6);
  y(1) = g / x(0);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf10(NumericVector x) {
  NumericVector y(2);
  y(0) = x(0);
  double e1 = exp(- pow((x(1) - 0.2) / 0.004, 2));
  double e2 = exp(- pow((x(1) - 0.6) / 0.4, 2));
  double g = 2 - e1 - 0.8 * e2;
  y(1) = g / x(0);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf11(NumericVector x, int np) {
  NumericVector y(2);
  y(0) = x(0);
  double tmp1 = exp(- 2 * log10(2) * pow((x(1) - 0.1) / 0.8, 2));
  double tmp2 = pow(sin(np * M_PI * x(1)), 6);
  double g = 2 - tmp1 * tmp2;
  y(1) = g / x(0);
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf12(NumericVector x, int np, int q) {
  NumericVector y(2);
  y(0) = x(0);
  double tmp1 = exp(- 2 * log10(2) * pow((x(1) - 0.1) / 0.8, 2));
  double tmp2 = pow(sin(np * M_PI * x(1)), 6);
  double g = 2 - tmp1 * tmp2;
  double h = 1 - pow(x(0) / g, 2) - (x(0) / g) * sin(2 * M_PI * q * x(0));
  y(1) = g * h;
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf13(NumericVector x, int np) {
  NumericVector y(2);
  y(0) = x(0);
  double tmp1 = x(1) + sqrt(x(2));
  double g = 2 - exp(- 2 * log10(2) * pow((tmp1 - 0.1) / 0.8, 2)) * pow(sin(np * M_PI * tmp1), 6);
  y(1) = g / x(0);
  return y;
}



// [[Rcpp::export]]
NumericVector mof_cec2019_mmf14(NumericVector x, int M, int np) {
  NumericVector y(M);
  int n = x.size();
  double g = 3 - pow(sin(np * M_PI * x(n - 1)), 2);
  for (int i = 0; i < M - 1; i++) {
    y(M - 1 - i) = g * sin(0.5 * M_PI * x(i));
    g = g * cos(0.5 * M_PI * x(i));
  }
  y(0) = g;
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf14_a(NumericVector x, int M, int np) {
  NumericVector y(M);
  int n = x.size();
  double tmp = x(n - 1) - 0.5 * sin(M_PI * x(n - 2)) + (0.5 / np);
  double g = 3.0 - pow(sin(np * M_PI * tmp), 2);
  for (int i = 0; i < M - 1; i++) {
    y(M - 1 - i) = g * sin(0.5 * M_PI * x(i));
    g = g * cos(0.5 * M_PI * x(i));
  }
  y(0) = g;
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf15(NumericVector x, int M, int np) {
  NumericVector y(M);
  int n = x.size();
  double g = 3.0 - exp(-2.0 * log10(2.0) * pow((x(n - 1) - 0.1) / 0.8, 2)) * pow(sin(np * M_PI * x(n - 1)), 2);
  for (int i = 0; i < M - 1; i++) {
    y(M - 1 - i) = g * sin(0.5 * M_PI * x(i));
    g = g * cos(0.5 * M_PI * x(i));
  }
  y(0) = g;
  return y;
}


// [[Rcpp::export]]
NumericVector mof_cec2019_mmf15_a(NumericVector x, int M, int np) {
  NumericVector y(M);
  int n = x.size();
  double t = x(n - 1) - 0.5 * sin(M_PI * x(n - 2)) + (0.5 / np);
  double g = 3.0 - exp(-2.0 * log10(2.0) * pow((t - 0.1) / 0.8, 2)) * pow(sin(np * M_PI * t), 2);
  for (int i = 0; i < M - 1; i++) {
    y(M - 1 - i) = g * sin(0.5 * M_PI * x(i));
    g = g * cos(0.5 * M_PI * x(i));
  }
  y(0) = g;
  return y;
}

