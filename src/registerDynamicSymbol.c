// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "test.h"
#include "EuclideanDistanceTransformation.h"

static R_NativePrimitiveArgType myC_type[] = {
   INTSXP,INTSXP,INTSXP,INTSXP
};
static const R_CMethodDef cMethods[] = {
   {"test",  (DL_FUNC) &test, 0},
   {"EuclideanDistanceTransformation", (DL_FUNC) &EuclideanDistanceTransformation, 4,myC_type},
   NULL
};

void R_init_topotoolboxr(DllInfo *info) {
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info,
                    cMethods, NULL,
                    NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
