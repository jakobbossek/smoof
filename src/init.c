#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP evaluateBBOBFunctionCPP(SEXP, SEXP, SEXP, SEXP);
extern SEXP evaluateUFFunction(SEXP, SEXP, SEXP);
extern SEXP getOptimumForBBOBFunctionCPP(SEXP, SEXP, SEXP);
extern SEXP mof_bk1(SEXP);
extern SEXP mof_MOP1(SEXP);
extern SEXP mof_MOP2(SEXP);
extern SEXP mof_MOP3(SEXP);
extern SEXP mof_MOP4(SEXP);
extern SEXP mof_MOP5(SEXP);
extern SEXP mof_MOP6(SEXP);
extern SEXP mof_MOP7(SEXP);
extern SEXP mof_viennet(SEXP);
extern SEXP smoof_dtlz_1(SEXP, SEXP);
extern SEXP smoof_dtlz_2(SEXP, SEXP);
extern SEXP smoof_dtlz_3(SEXP, SEXP);
extern SEXP smoof_dtlz_4(SEXP, SEXP, SEXP);
extern SEXP smoof_dtlz_5(SEXP, SEXP);
extern SEXP smoof_dtlz_6(SEXP, SEXP);
extern SEXP smoof_dtlz_7(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"evaluateBBOBFunctionCPP",      (DL_FUNC) &evaluateBBOBFunctionCPP,      4},
    {"evaluateUFFunction",           (DL_FUNC) &evaluateUFFunction,           3},
    {"getOptimumForBBOBFunctionCPP", (DL_FUNC) &getOptimumForBBOBFunctionCPP, 3},
    {"mof_bk1",                      (DL_FUNC) &mof_bk1,                      1},
    {"mof_MOP1",                     (DL_FUNC) &mof_MOP1,                     1},
    {"mof_MOP2",                     (DL_FUNC) &mof_MOP2,                     1},
    {"mof_MOP3",                     (DL_FUNC) &mof_MOP3,                     1},
    {"mof_MOP4",                     (DL_FUNC) &mof_MOP4,                     1},
    {"mof_MOP5",                     (DL_FUNC) &mof_MOP5,                     1},
    {"mof_MOP6",                     (DL_FUNC) &mof_MOP6,                     1},
    {"mof_MOP7",                     (DL_FUNC) &mof_MOP7,                     1},
    {"mof_viennet",                  (DL_FUNC) &mof_viennet,                  1},
    {"smoof_dtlz_1",                 (DL_FUNC) &smoof_dtlz_1,                 2},
    {"smoof_dtlz_2",                 (DL_FUNC) &smoof_dtlz_2,                 2},
    {"smoof_dtlz_3",                 (DL_FUNC) &smoof_dtlz_3,                 2},
    {"smoof_dtlz_4",                 (DL_FUNC) &smoof_dtlz_4,                 3},
    {"smoof_dtlz_5",                 (DL_FUNC) &smoof_dtlz_5,                 2},
    {"smoof_dtlz_6",                 (DL_FUNC) &smoof_dtlz_6,                 2},
    {"smoof_dtlz_7",                 (DL_FUNC) &smoof_dtlz_7,                 2},
    {NULL, NULL, 0}
};

void R_init_smoof(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
