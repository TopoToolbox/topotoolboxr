// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "has_topotoolbox.h"

static R_NativePrimitiveArgType myC_type[] = {
   INTSXP,INTSXP,INTSXP,INTSXP,INTSXP,INTSXP
};
static const R_CMethodDef cMethods[] = {
   {"has_topotoolbox", (DL_FUNC) &has_topotoolbox,0},
   {NULL},
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
