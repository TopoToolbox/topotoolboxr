#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_gwdt_computecosts(float *costsR,
                            int *conncompsR,
                            int *flatsR,
                            float *original_demR,
                            float *filled_demR,
                            int *dimsR){
  
  // Transform integers from R to ptrdiff_t
  ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};
  
  ptrdiff_t *conncomps = R_Calloc(dims[0] * dims[1], ptrdiff_t);
  
  for (ptrdiff_t j = 0; j < dims[1]; j++) {
    for (ptrdiff_t i = 0; i < dims[0]; i++) {
      conncomps[j * dims[0] + i] = conncompsR[j * dims[0] + i];
    }
  }

  // Cost computation using libtopotoolbox
  gwdt_computecosts(costsR,
                    conncomps,
                    flatsR,
                    original_demR,
                    filled_demR,
                    dims);
  
  // Return conncomps as conncompsR
  for (ptrdiff_t j = 0; j < dims[1]; j++) {
    for (ptrdiff_t i = 0; i < dims[0]; i++) {
      conncompsR[j * dims[0] + i] = (int)conncomps[j * dims[0] + i];
    }
  }

  R_Free(conncomps);
}
