/*
 * R to C interface for the CEC2009-MOEA test functions UF1, ..., UF10.
 *
 * @author Jakob Bossek <j.bossek@gmail.com>
 *
 * Based on the C++ implementation of Qingfu Zhang,
 * Aimin Zhou, Shizheng Zhaoy, Ponnuthurai Nagaratnam Suganthany, Wudong Liu and Santosh Tiwar.
 *
*/

// load R headers
#include <stdio.h>
#include <R.h>
#include <Rinternals.h>
#include "cec09.h"
#include <math.h>

#include "macros.h"

#if defined(PI)
#undef PI
#endif

#define PI  3.1415926535897932384626433832795
#define MYSIGN(x) ((x)>0?1.0:-1.0)

void UF1(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2;
  double sum1, sum2, yj;

  sum1   = sum2   = 0.0;
  count1 = count2 = 0;
  for(j = 2; j <= nx; j++)
  {
   yj = x[j-1] - sin(6.0*PI*x[0] + j*PI/nx);
   yj = yj * yj;
   if(j % 2 == 0)
   {
    sum2 += yj;
    count2++;
  }
  else
  {
    sum1 += yj;
    count1++;
  }
}
f[0] = x[0]				+ 2.0 * sum1 / (double)count1;
f[1] = 1.0 - sqrt(x[0]) + 2.0 * sum2 / (double)count2;
}

void UF2(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2;
  double sum1, sum2, yj;

  sum1   = sum2   = 0.0;
  count1 = count2 = 0;
  for(j = 2; j <= nx; j++)
  {
   if(j % 2 == 0)
   {
    yj = x[j-1]-0.3*x[0]*(x[0]*cos(4.0*(6.0*PI*x[0]+j*PI/nx))+2.0)*cos(6.0*PI*x[0]+j*PI/nx);
    sum2 += yj*yj;
    count2++;
  }
  else
  {
    yj = x[j-1]-0.3*x[0]*(x[0]*cos(4.0*(6.0*PI*x[0]+j*PI/nx))+2.0)*sin(6.0*PI*x[0]+j*PI/nx);
    sum1 += yj*yj;
    count1++;
  }
}
f[0] = x[0]				+ 2.0 * sum1 / (double)count1;
f[1] = 1.0 - sqrt(x[0]) + 2.0 * sum2 / (double)count2;
}

void UF3(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2;
  double sum1, sum2, prod1, prod2, yj, pj;

  sum1   = sum2   = 0.0;
  count1 = count2 = 0;
  prod1  = prod2  = 1.0;
  for(j = 2; j <= nx; j++)
  {
   yj = x[j-1]-pow(x[0],0.5*(1.0+3.0*(j-2.0)/(nx-2.0)));
   pj = cos(20.0*yj*PI/sqrt(j+0.0));
   if (j % 2 == 0)
   {
    sum2  += yj*yj;
    prod2 *= pj;
    count2++;
  }
  else
  {
    sum1  += yj*yj;
    prod1 *= pj;
    count1++;
  }
}
f[0] = x[0]				+ 2.0*(4.0*sum1 - 2.0*prod1 + 2.0) / (double)count1;
f[1] = 1.0 - sqrt(x[0]) + 2.0*(4.0*sum2 - 2.0*prod2 + 2.0) / (double)count2;
}

void UF4(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2;
  double sum1, sum2, yj, hj;

  sum1   = sum2   = 0.0;
  count1 = count2 = 0;
  for(j = 2; j <= nx; j++)
  {
   yj = x[j-1]-sin(6.0*PI*x[0]+j*PI/nx);
   hj = fabs(yj)/(1.0+exp(2.0*fabs(yj)));
   if (j % 2 == 0)
   {
    sum2  += hj;
    count2++;
  }
  else
  {
    sum1  += hj;
    count1++;
  }
}
f[0] = x[0]				+ 2.0*sum1 / (double)count1;
f[1] = 1.0 - x[0]*x[0]	+ 2.0*sum2 / (double)count2;
}

void UF5(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2;
  double sum1, sum2, yj, hj, N, E;

  sum1   = sum2   = 0.0;
  count1 = count2 = 0;
  N = 10.0; E = 0.1;
  for(j = 2; j <= nx; j++)
  {
   yj = x[j-1] - sin(6.0*PI*x[0]);
   hj = 2.0*yj*yj - cos(4.0*PI*yj) + 1.0;
   if (j % 2 == 0)
   {
    sum2  += hj;
    count2++;
  }
  else
  {
    sum1  += hj;
    count1++;
  }
}
hj = 0.5*(0.5/N + E)*fabs(sin(2.0*N*PI*x[0]));
f[0] = x[0]	      + hj + 2.0*sum1 / (double)count1;
f[1] = 1.0 - x[0] + hj + 2.0*sum2 / (double)count2;
}

void UF6(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2;
  double sum1, sum2, yj, hj, N, E;

  sum1   = sum2   = 0.0;
  count1 = count2 = 0;
  N = 2.0; E = 0.1;
  for(j = 2; j <= nx; j++)
  {
   yj = x[j-1] - sin(6.0*PI*x[0]+j*PI/nx);
   if (j % 2 == 0)
   {
    sum2  += yj*yj;
    count2++;
  }
  else
  {
    sum1  += yj*yj;
    count1++;
  }
}
hj = 0.5*(0.5/N + E)*sin(2.0*N*PI*x[0]);
if(hj<0.0) hj = 0.0;
f[0] = x[0]	      + hj + 2.0*sum1 / (double)count1;
f[1] = 1.0 - x[0] + hj + 2.0*sum2 / (double)count2;
}

void UF7(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2;
  double sum1, sum2, yj;

  sum1   = sum2   = 0.0;
  count1 = count2 = 0;
  for(j = 2; j <= nx; j++)
  {
   yj = x[j-1] - sin(6.0*PI*x[0]+j*PI/nx);
   if (j % 2 == 0)
   {
    sum2  += yj*yj;
    count2++;
  }
  else
  {
    sum1  += yj*yj;
    count1++;
  }
}
yj = pow(x[0],0.2);
f[0] = yj	    + 2.0*sum1 / (double)count1;
f[1] = 1.0 - yj + 2.0*sum2 / (double)count2;
}

void UF8(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2, count3;
  double sum1, sum2, sum3, yj;

  sum1   = sum2   = sum3   = 0.0;
  count1 = count2 = count3 = 0;
  for(j = 3; j <= nx; j++)
  {
   yj = x[j-1] - 2.0*x[1]*sin(2.0*PI*x[0]+j*PI/nx);
   if(j % 3 == 1)
   {
    sum1  += yj*yj;
    count1++;
  }
  else if(j % 3 == 2)
  {
    sum2  += yj*yj;
    count2++;
  }
  else
  {
    sum3  += yj*yj;
    count3++;
  }
}
f[0] = cos(0.5*PI*x[0])*cos(0.5*PI*x[1]) + 2.0*sum1 / (double)count1;
f[1] = cos(0.5*PI*x[0])*sin(0.5*PI*x[1]) + 2.0*sum2 / (double)count2;
f[2] = sin(0.5*PI*x[0])                  + 2.0*sum3 / (double)count3;
}

void UF9(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2, count3;
  double sum1, sum2, sum3, yj, E;

  E = 0.1;
  sum1   = sum2   = sum3   = 0.0;
  count1 = count2 = count3 = 0;
  for(j = 3; j <= nx; j++)
  {
   yj = x[j-1] - 2.0*x[1]*sin(2.0*PI*x[0]+j*PI/nx);
   if(j % 3 == 1)
   {
    sum1  += yj*yj;
    count1++;
  }
  else if(j % 3 == 2)
  {
    sum2  += yj*yj;
    count2++;
  }
  else
  {
    sum3  += yj*yj;
    count3++;
  }
}
yj = (0.5+E)*(1.0-4.0*(2.0*x[0]-1.0)*(2.0*x[0]-1.0));
if(yj<0.0) yj = 0.0;
f[0] = 0.5*(yj + 2*x[0])*x[1]		+ 2.0*sum1 / (double)count1;
f[1] = 0.5*(yj - 2*x[0] + 2.0)*x[1] + 2.0*sum2 / (double)count2;
f[2] = 1.0 - x[1]                   + 2.0*sum3 / (double)count3;
}

void UF10(double *x, double *f, const unsigned int nx)
{
  unsigned int j, count1, count2, count3;
  double sum1, sum2, sum3, yj, hj;

  sum1   = sum2   = sum3   = 0.0;
  count1 = count2 = count3 = 0;
  for(j = 3; j <= nx; j++)
  {
    yj = x[j-1] - 2.0*x[1]*sin(2.0*PI*x[0]+j*PI/nx);
    hj = 4.0*yj*yj - cos(8.0*PI*yj) + 1.0;
    if(j % 3 == 1)
    {
    sum1  += hj;
    count1++;
    }
    else if(j % 3 == 2)
    {
      sum2  += hj;
      count2++;
    }
    else
    {
      sum3  += hj;
      count3++;
    }
  }
  f[0] = cos(0.5*PI*x[0])*cos(0.5*PI*x[1]) + 2.0*sum1 / (double)count1;
  f[1] = cos(0.5*PI*x[0])*sin(0.5*PI*x[1]) + 2.0*sum2 / (double)count2;
  f[2] = sin(0.5*PI*x[0])                  + 2.0*sum3 / (double)count3;
}
