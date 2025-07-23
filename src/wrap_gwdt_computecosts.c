#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_gwdt_computecosts(float *costsR,
                            int *flatsR,
                            float *original_demR,
                            float *filled_demR,
                            int *dimsR){
  // Transform integers from R to ptrdiff_t
  ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};
  ptrdiff_t *conncomps = R_Calloc(dims[0] * dims[1], ptrdiff_t);

  // Cost computation using libtopotoolbox
  gwdt_computecosts(costsR,
                    conncomps,
                    flatsR,
                    original_demR,
                    filled_demR,
                    dims);
  // Free memory
  R_Free(conncomps);
}
