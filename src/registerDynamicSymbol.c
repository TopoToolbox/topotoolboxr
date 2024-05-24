// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "test.h"

static const R_CMethodDef cMethods[] = {
   {"test",  (DL_FUNC) &test, 0},
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
