#include <stdio.h>

const void hello(){
	printf("hello world"); // writing to stdout is not allowed on cran therfore a note on check() and reference to 'Writing R Extensions'
}

#include <R_ext/Rdynload.h>

static const R_CMethodDef cMethods[] = {
   {"hello",  (DL_FUNC) &hello, 0},
   NULL
};

void R_init_myRoutines(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
 R_registerRoutines(info,
                    cMethods, NULL,
                    NULL, NULL);
}
