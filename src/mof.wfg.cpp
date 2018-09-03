#include <RcppArmadillo.h>
#include <math.h>
#include <algorithm>
using namespace Rcpp;

NumericVector WFG_norm_z(NumericVector z){
  NumericVector result;
  int n = z.size();
  for (int i = 0; i <  n; i++) {
    double bound = 2.0 * (i + 1);
    result.push_back( z[i] / bound );
  }
  return result;
}

NumericVector WFG_normalize_z(NumericVector z){
  // normalization as described in caption of Table XIV
  int n = z.size();
  NumericVector result(n);
  for (int i = 0; i < n; i++) {
    result(i) = z(i) / (2.0 * (i + 1.0));
  }
  return result;
}


// SHAPE FUNCTIONS, see Table X from IEEE TEVC paper 

double WFG_shape_linear(NumericVector x, int M, int m){
  // linear shape as in first block of Table X
  double result = 1.0;
  int i;
  
  if (m == 1) {
    // linear_1
    for (i = 1; i < M; i++) {
      result *= x(i - 1);
    }
  } else if (m < M) {
    // linear_m with m = 2, ..., (M - 1)
    for (i = 1; i <= M - m; i++) {
      result *= x(i - 1);
    }
    result *= (1 - x(M - m));
  } else if (m == M) {
    // linear_M
    result = 1 - x(0);
  }
  return result;
}

double WFG_shape_convex(NumericVector x, int M, int m){
  // convex shape as in second block of Table X
  double result = 1.0;
  int i;
  
  if (m == 1) {
    // convex_1
    for (i = 1; i < M; i++) {
      result *= (1 - cos(x(i - 1) * M_PI / 2.0));
    }
  } else if (m < M) {
    // convex_m with m = 2, ..., (M - 1)
    for (i = 1; i <= M - m; i++) {
      result *= (1 - cos(x(i - 1) * M_PI / 2.0));
    }
    result *= (1 - sin(x(M - m) * M_PI / 2.0));
  } else if (m == M) {
    // convex_M
    result = 1 - sin(x(0) * M_PI / 2.0);
  }
  return result;
}

double WFG_shape_concave(NumericVector x, int M, int m){
  // concave shape as in third block of Table X
  double result = 1.0;
  int i;
  
  if (m == 1) {
    // concave_1
    for (i = 1; i < M; i++) {
      result *= sin(x(i - 1) * M_PI / 2.0);
    }
  } else if (m < M) {
    // concave_m with m = 2, ..., (M - 1)
    for (i = 1; i <= M - m; i++) {
      result *= sin(x(i - 1) * M_PI / 2.0);
    }
    result *= cos(x(M - m) * M_PI / 2.0);
  } else if (m == M) {
    // concave_M
    result = cos(x(0) * M_PI / 2.0);
  }
  return result;
}

double WFG_shape_mixed(NumericVector x, double alpha, int A){
  // mixed shape as in fourth block of Table X
  double result;

  result = pow(1 - x(0) - (cos(2.0 * A * M_PI * x(0) + M_PI / 2.0)) / (2.0 * A * M_PI), alpha);
  return result;
}

double WFG_shape_disc(NumericVector x, double alpha, double beta, int A){
  // disconnected shape as in fifth block of Table X
  double result;
  
  result = 1 - pow(x(0), alpha) * pow(cos(A * pow(x(0), beta) * M_PI), 2.0);
  return result;
}


// TRANSFORMATION FUNCTIONS, see Table XI from IEEE TEVC paper 


double WFG_trafo_bias_polynomial(double y, double alpha){
  // polynomial bias, see first block of Table XI
  double result = pow(y, alpha);
  return result;
}

double WFG_trafo_bias_flat(double y, double A, double B, double C){
  // flat region bias, see second block of Table XI
  double result = - std::min(0.0, floor(C - y)) * ((1.0 - A) * (y - C) / (1.0 - C));
  result += A + std::min(0.0, floor(y - B)) * (A * (B - y) / B);
  return result;
}

double WFG_trafo_bias_param(double y, double u, double A, double B, double C){
  // parameter dependent bias, see third block of Table XI
  double v = A - (1.0 - 2.0 * u) * fabs(floor(0.5 - u) + A);
  double result = pow(y, B + (C - B) * v);
  return result;
}

double WFG_trafo_shift_linear(double y, double A){
  // linear shift, see fourth block of Table XI
  double result = fabs(y - A) / fabs(floor(A - y) + A);
  return result;
}

double WFG_trafo_shift_deceptive(double y, double A, double B, double C){
  // deceptive shift, see fifth block of Table XI
  double result = floor(y - A + B) * (1.0 - C + (A - B) / B) / (A - B);
  result += floor(A + B - y) * (1 - C + (1.0 - A - B) / B) / (1.0 - A - B);
  result += (1.0 / B);
  result *= (fabs(y - A) - B);
  result += 1.0;
  return result;
}

double WFG_trafo_shift_multimodal(double y, double A, double B, double C){
  // multimodal shift, see sixth block of Table XI
  double result = 0.5 - fabs(y - C) / (2.0 * (floor(C - y) + C));
  result = 1.0 + cos((4.0 * A + 2.0) * M_PI * result);
  result += 4.0 * B * pow(fabs(y - C) / (2.0 * (floor(C - y) + C)), 2.0);
  result = result / (B + 2.0);
  return result;
}

double WFG_trafo_reduction_weighted_sum(NumericVector y, NumericVector w){
  // weighted sum reduction, see seventh block of Table XI
  int n = y.size();
  double a;
  double b;
  for (int i = 0; i < n; i++) {
    a += w(i) * y(i);
    b += w(i);
  }
  double result = a / b;
  return result;
}

double WFG_trafo_reduction_nonseparable(NumericVector y, int A){
  // non-separable reduction, see eighth block of Table XI
  int n = y.size();
  double numerator = 0.0;
  for (int j = 0; j < n; j++) {
    numerator += y(j);
    for (int k = 0; k < A - 1; k++) {
      numerator += fabs(y(j) - y((j + k + 1) % n));
    }
  }
  double denominator = (fabs(n) / A) * ceil(A / 2.0) * (1.0 + 2 * A - 2.0 * ceil(A / 2.0));
  double result = numerator / denominator;
  return result;
}

NumericVector subvector(NumericVector v, int head, int tail) {
  NumericVector result(tail + 1 - head);
  for (int i = head; i <= tail; i++) {
    result(i - head) = v(i - 1);
  }
  return(result);
}

NumericVector WFG_calc_x(NumericVector t, NumericVector A) {
  int M = t.size();
  NumericVector x(M);
  for (int i = 0; i < M - 1; i++) {
    x(i) = std::max(t(M - 1), A(i)) * (t(i) - 0.5) + 0.5;
  }
  x(M - 1) = t(M - 1);
  return x;
}


// [[Rcpp::export]]
NumericVector mof_WFG_1(NumericVector z, int M, int k) {
  int i;
  int m;
  int n = z.size();
  NumericVector A(M - 1, 1.0);

  NumericVector S(M);
  for (m = 0; m < M; m++) {
    S(m) = 2.0 * (m + 1.0);
  }
  int D = 1;

  NumericVector y(n);
  NumericVector t4(M);
  NumericVector fi(M);
  NumericVector x(M);
  NumericVector h(M);

  // normalize z
  y = WFG_normalize_z(z);

  for (i = k; i < n; i++) {
    // transformation t1
    y(i) = WFG_trafo_shift_linear(y(i), 0.35);
    // transformation t2
    y(i) = WFG_trafo_bias_flat(y(i), 0.8, 0.75, 0.85);
  }

  // transformation t3
  for (i = 0; i < n; i++) {
    y(i) = WFG_trafo_bias_polynomial(y(i), 0.02);
  }

  // transformation t4
  NumericVector w(n);
  for (i = 0; i < n; i++) {
    w(i) = 2.0 * (i + 1.0);
  }
  int tmp = (k / (M - 1));
  for (i = 1; i < M; i++) {
    int head = (i - 1) * tmp + 1;
    int tail = i * tmp;
    NumericVector ysub = subvector(y, head, tail);
    NumericVector wsub = subvector(w, head, tail);
    t4(i - 1) = WFG_trafo_reduction_weighted_sum(ysub, wsub);
  }
  int head = k + 1;
  int tail = n;
  NumericVector ysub = subvector(y, head, tail);
  NumericVector wsub = subvector(w, head, tail);
  t4(M - 1) = WFG_trafo_reduction_weighted_sum(ysub, wsub);

  // define x
  x = WFG_calc_x(t4, A);

  // compute h
  for (m = 1; m < M; m++) {
    h(m - 1) = WFG_shape_convex(x, M, m);
  }
  h(M - 1) = WFG_shape_mixed(x, 1.0, 5);

  // now, the actual objectives can be computed
  for (m = 1; m <= M; m++) {
    fi(m - 1) = D * x(M - 1) + S(m - 1) * h(m - 1);
  }

  return fi;
}


// [[Rcpp::export]]
NumericVector mof_WFG_2(NumericVector z, int M, int k) {
  // Attention: n - k needs to be an even number (!)
  int i;
  int m;
  int n = z.size();
  int l2 = int((n - k)/2);
  NumericVector A(M - 1, 1.0);

  NumericVector S(M);
  for (m = 0; m < M; m++) {
    S(m) = 2.0 * (m + 1.0);
  }
  int D = 1;

  NumericVector y(n);
  NumericVector t2(k + l2);
  NumericVector t3(M);
  NumericVector fi(M);
  NumericVector x(M);
  NumericVector h(M);

  // normalize z
  y = WFG_normalize_z(z);

  // transformation t1
  for (i = k; i < n; i++) {
    y(i) = WFG_trafo_shift_linear(y(i), 0.35);
  }

  // transformation t2
  for (i = 0; i < k; i++) {
    t2(i) = y(i);
  }
  for (i = (k + 1); i <= (k + (l2)); i++) {
    NumericVector ysub = subvector(y, k + 2*(i - k) - 1, k + 2*(i - k));
    t2(i - 1) = WFG_trafo_reduction_nonseparable(ysub, 2);
  }

  // transformation t3
  int tmp = (k / (M - 1));
  NumericVector w(tmp, 1.0);
  for (i = 1; i < M; i++) {
    int head = (i - 1) * tmp + 1;
    int tail = i * tmp;
    NumericVector ysub = subvector(t2, head, tail);
    t3(i - 1) = WFG_trafo_reduction_weighted_sum(ysub, w);
  }
  NumericVector w2(l2, 1.0);
  NumericVector ysub = subvector(t2, k + 1, k + (l2));
  t3(M - 1) = WFG_trafo_reduction_weighted_sum(ysub, w2);

  // define x
  x = WFG_calc_x(t3, A);

  // compute h
  for (m = 1; m < M; m++) {
    h(m - 1) = WFG_shape_convex(x, M, m);
  }
  h(M - 1) = WFG_shape_disc(x, 1.0, 1.0, 5);

  // now, the actual objectives can be computed
  for (m = 1; m <= M; m++) {
    fi(m - 1) = D * x(M - 1) + S(m - 1) * h(m - 1);
  }

  return fi;
}


// [[Rcpp::export]]
NumericVector mof_WFG_3(NumericVector z, int M, int k) {
  // Attention: n - k needs to be an even number (!)
  int i;
  int m;
  int n = z.size();
  int l2 = int((n - k)/2);
  NumericVector A(M - 1);
  A(0) = 1.0;

  NumericVector S(M);
  for (m = 0; m < M; m++) {
    S(m) = 2.0 * (m + 1.0);
  }
  int D = 1;

  NumericVector y(n);
  NumericVector t2(k + l2);
  NumericVector t3(M);
  NumericVector fi(M);
  NumericVector x(M);
  NumericVector h(M);

  // normalize z
  y = WFG_normalize_z(z);

  // transformation t1
  for (i = k; i < n; i++) {
    y(i) = WFG_trafo_shift_linear(y(i), 0.35);
  }

  // transformation t2
  for (i = 0; i < k; i++) {
    t2(i) = y(i);
  }
  for (i = (k + 1); i <= (k + l2); i++) {
    NumericVector ysub = subvector(y, k + 2*(i - k) - 1, k + 2*(i - k));
    t2(i - 1) = WFG_trafo_reduction_nonseparable(ysub, 2);
  }

  // transformation t3
  int tmp = (k / (M - 1));
  NumericVector w(tmp, 1.0);
  for (i = 1; i < M; i++) {
    int head = (i - 1) * tmp + 1;
    int tail = i * tmp;
    NumericVector ysub = subvector(t2, head, tail);
    t3(i - 1) = WFG_trafo_reduction_weighted_sum(ysub, w);
  }
  NumericVector w2(l2, 1.0);
  NumericVector ysub = subvector(t2, k + 1, k + (l2));
  t3(M - 1) = WFG_trafo_reduction_weighted_sum(ysub, w2);

  // define x
  x = WFG_calc_x(t3, A);

  // compute h
  for (m = 1; m <= M; m++) {
    h(m - 1) = WFG_shape_linear(x, M, m);
  }

  // now, the actual objectives can be computed
  for (m = 1; m <= M; m++) {
    fi(m - 1) = D * x(M - 1) + S(m - 1) * h(m - 1);
  }

  return fi;
  return x;
}


// [[Rcpp::export]]
NumericVector mof_WFG_4(NumericVector z, int M, int k) {
  int i;
  int m;
  int n = z.size();
  NumericVector A(M - 1, 1.0);

  NumericVector S(M);
  for (m = 0; m < M; m++) {
    S(m) = 2.0 * (m + 1.0);
  }
  int D = 1;

  NumericVector y(n);
  NumericVector t2(M);
  NumericVector fi(M);
  NumericVector x(M);
  NumericVector h(M);

  // normalize z
  y = WFG_normalize_z(z);

  // transformation t1
  for (i = 0; i < n; i++) {
    y(i) = WFG_trafo_shift_multimodal(y(i), 30, 10, 0.35);
  }

  // transformation t2
  int tmp = (k / (M - 1));
  NumericVector w(tmp, 1.0);
  for (i = 1; i < M; i++) {
    int head = (i - 1) * tmp + 1;
    int tail = i * tmp;
    NumericVector ysub = subvector(y, head, tail);
    t2(i - 1) = WFG_trafo_reduction_weighted_sum(ysub, w);
  }
  NumericVector ysub = subvector(y, k + 1, n);
  NumericVector w2(ysub.size(), 1.0);
  t2(M - 1) = WFG_trafo_reduction_weighted_sum(ysub, w2);

  // define x
  x = WFG_calc_x(t2, A);

  // compute h
  for (m = 1; m <= M; m++) {
    h(m - 1) = WFG_shape_concave(x, M, m);
  }

  // now, the actual objectives can be computed
  for (m = 1; m <= M; m++) {
    fi(m - 1) = D * x(M - 1) + S(m - 1) * h(m - 1);
  }

  return fi;
}


// [[Rcpp::export]]
NumericVector mof_WFG_5(NumericVector z, int M, int k) {
  int i;
  int m;
  int n = z.size();
  NumericVector A(M - 1, 1.0);

  NumericVector S(M);
  for (m = 0; m < M; m++) {
    S(m) = 2.0 * (m + 1.0);
  }
  int D = 1;

  NumericVector y(n);
  NumericVector t2(M);
  NumericVector fi(M);
  NumericVector x(M);
  NumericVector h(M);

  // normalize z
  y = WFG_normalize_z(z);

  // transformation t1
  for (i = 0; i < n; i++) {
    y(i) = WFG_trafo_shift_deceptive(y(i), 0.35, 0.001, 0.05);
  }

  // transformation t2
  int tmp = (k / (M - 1));
  NumericVector w(tmp, 1.0);
  for (i = 1; i < M; i++) {
    int head = (i - 1) * tmp + 1;
    int tail = i * tmp;
    NumericVector ysub = subvector(y, head, tail);
    t2(i - 1) = WFG_trafo_reduction_weighted_sum(ysub, w);
  }
  NumericVector ysub = subvector(y, k + 1, n);
  NumericVector w2(ysub.size(), 1.0);
  t2(M - 1) = WFG_trafo_reduction_weighted_sum(ysub, w2);

  // define x
  x = WFG_calc_x(t2, A);

  // compute h
  for (m = 1; m <= M; m++) {
    h(m - 1) = WFG_shape_concave(x, M, m);
  }

  // now, the actual objectives can be computed
  for (m = 1; m <= M; m++) {
    fi(m - 1) = D * x(M - 1) + S(m - 1) * h(m - 1);
  }

  return fi;
}


// [[Rcpp::export]]
NumericVector mof_WFG_6(NumericVector z, int M, int k) {
  int i;
  int m;
  int n = z.size();
  int l = n - k;
  NumericVector A(M - 1, 1.0);

  NumericVector S(M);
  for (m = 0; m < M; m++) {
    S(m) = 2.0 * (m + 1.0);
  }
  int D = 1;

  NumericVector y(n);
  NumericVector t2(M);
  NumericVector fi(M);
  NumericVector x(M);
  NumericVector h(M);

  // normalize z
  y = WFG_normalize_z(z);

  // transformation t1
  for (i = k; i < n; i++) {
    y(i) = WFG_trafo_shift_linear(y(i), 0.35);
  }

  // transformation t2
  int tmp = (k / (M - 1));
  for (i = 1; i <= (M - 1); i++) {
    NumericVector ysub = subvector(y, (i - 1) * tmp + 1, i * tmp);
    t2(i - 1) = WFG_trafo_reduction_nonseparable(ysub, tmp);
  }
  NumericVector ysub = subvector(y, k + 1, n);
  t2(M - 1) = WFG_trafo_reduction_nonseparable(ysub, l);

  // define x
  x = WFG_calc_x(t2, A);

  // compute h
  for (m = 1; m <= M; m++) {
    h(m - 1) = WFG_shape_concave(x, M, m);
  }

  // now, the actual objectives can be computed
  for (m = 1; m <= M; m++) {
    fi(m - 1) = D * x(M - 1) + S(m - 1) * h(m - 1);
  }

  return fi;
}


// [[Rcpp::export]]
NumericVector mof_WFG_7(NumericVector z, int M, int k) {
  int i;
  int m;
  int n = z.size();
  int l = n - k;
  NumericVector A(M - 1, 1.0);

  NumericVector S(M);
  for (m = 0; m < M; m++) {
    S(m) = 2.0 * (m + 1.0);
  }
  int D = 1;

  NumericVector y(n);
  NumericVector t3(M);
  NumericVector fi(M);
  NumericVector x(M);
  NumericVector h(M);

  // normalize z
  y = WFG_normalize_z(z);

  // transformation t1
  for (i = 1; i <= k; i++) {
    NumericVector ysub = subvector(y, i + 1, n);
    NumericVector w(ysub.size(), 1.0);
    double rs = WFG_trafo_reduction_weighted_sum(ysub, w);
    y(i - 1) = WFG_trafo_bias_param(y(i - 1), rs, 0.98/49.98, 0.02, 50);
  }

  // transformation t2
  for (i = k; i < n; i++) {
    y(i) = WFG_trafo_shift_linear(y(i), 0.35);
  }

  // transformation t3
  int tmp = (k / (M - 1));
  NumericVector w(tmp, 1.0);
  for (i = 1; i < M; i++) {
    int head = (i - 1) * tmp + 1;
    int tail = i * tmp;
    NumericVector ysub = subvector(y, head, tail);
    t3(i - 1) = WFG_trafo_reduction_weighted_sum(ysub, w);
  }
  NumericVector w2(l, 1.0);
  NumericVector ysub = subvector(y, k + 1, n);
  t3(M - 1) = WFG_trafo_reduction_weighted_sum(ysub, w2);

  // define x
  x = WFG_calc_x(t3, A);

  // compute h
  for (m = 1; m <= M; m++) {
    h(m - 1) = WFG_shape_concave(x, M, m);
  }

  // now, the actual objectives can be computed
  for (m = 1; m <= M; m++) {
    fi(m - 1) = D * x(M - 1) + S(m - 1) * h(m - 1);
  }

  return fi;
}


// [[Rcpp::export]]
NumericVector mof_WFG_8(NumericVector z, int M, int k) {
  int i;
  int m;
  int n = z.size();
  int l = n - k;
  NumericVector A(M - 1, 1.0);

  NumericVector S(M);
  for (m = 0; m < M; m++) {
    S(m) = 2.0 * (m + 1.0);
  }
  int D = 1;

  NumericVector y(n);
  NumericVector t3(M);
  NumericVector fi(M);
  NumericVector x(M);
  NumericVector h(M);

  // normalize z
  y = WFG_normalize_z(z);

  // transformation t1 (need to run in reversed order to avoid access of
  // elements that were update in the previous iteration of the loop)
  for (i = n; i >= (k + 1); i--) {
    NumericVector ysub = subvector(y, 1, i - 1);
    NumericVector w(ysub.size(), 1.0);
    double rs = WFG_trafo_reduction_weighted_sum(ysub, w);
    y(i - 1) = WFG_trafo_bias_param(y(i - 1), rs, 0.98/49.98, 0.02, 50);
  }

  // transformation t2
  for (i = k; i < n; i++) {
    y(i) = WFG_trafo_shift_linear(y(i), 0.35);
  }

  // transformation t3
  int tmp = (k / (M - 1));
  NumericVector w(tmp, 1.0);
  for (i = 1; i < M; i++) {
    int head = (i - 1) * tmp + 1;
    int tail = i * tmp;
    NumericVector ysub = subvector(y, head, tail);
    t3(i - 1) = WFG_trafo_reduction_weighted_sum(ysub, w);
  }
  NumericVector w2(l, 1.0);
  NumericVector ysub = subvector(y, k + 1, n);
  t3(M - 1) = WFG_trafo_reduction_weighted_sum(ysub, w2);

  // define x
  x = WFG_calc_x(t3, A);

  // compute h
  for (m = 1; m <= M; m++) {
    h(m - 1) = WFG_shape_concave(x, M, m);
  }

  // now, the actual objectives can be computed
  for (m = 1; m <= M; m++) {
    fi(m - 1) = D * x(M - 1) + S(m - 1) * h(m - 1);
  }

  return fi;
}


// [[Rcpp::export]]
NumericVector mof_WFG_9(NumericVector z, int M, int k) {
  int i;
  int m;
  int n = z.size();
  int l = n - k;
  NumericVector A(M - 1, 1.0);
  
  NumericVector S(M);
  for (m = 0; m < M; m++) {
    S(m) = 2.0 * (m + 1.0);
  }
  int D = 1;
  
  NumericVector y(n);
  NumericVector t3(M);
  NumericVector fi(M);
  NumericVector x(M);
  NumericVector h(M);
  
  // normalize z
  y = WFG_normalize_z(z);
  
  // transformation t1
  for (i = 1; i < n; i++) {
    NumericVector ysub = subvector(y, i + 1, n);
    NumericVector w(ysub.size(), 1.0);
    double rs = WFG_trafo_reduction_weighted_sum(ysub, w);
    y(i - 1) = WFG_trafo_bias_param(y(i - 1), rs, 0.98/49.98, 0.02, 50.0);
  }

  // transformation t2
  for (i = 1; i <= k; i++) {
    y(i - 1) = WFG_trafo_shift_deceptive(y(i - 1), 0.35, 0.001, 0.05);
  }
  for (i = k + 1; i <= n; i++) {
    y(i - 1) = WFG_trafo_shift_multimodal(y(i - 1), 30.0, 95.0, 0.35);
  }

  // transformation t3
  int tmp = (k / (M - 1));
  for (i = 1; i <= (M - 1); i++) {
    NumericVector ysub = subvector(y, (i - 1) * tmp + 1, i * tmp);
    t3(i - 1) = WFG_trafo_reduction_nonseparable(ysub, tmp);
  }
  NumericVector ysub = subvector(y, k + 1, n);
  t3(M - 1) = WFG_trafo_reduction_nonseparable(ysub, l);

  // define x
  x = WFG_calc_x(t3, A);

  // compute h
  for (m = 1; m <= M; m++) {
    h(m - 1) = WFG_shape_concave(x, M, m);
  }

  // now, the actual objectives can be computed
  for (m = 1; m <= M; m++) {
    fi(m - 1) = D * x(M - 1) + S(m - 1) * h(m - 1);
  }

  return fi;
}
