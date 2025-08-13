#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"

#include "topotoolboxr.h"

void wrap_fillsinks(float *output, float *dem, int *bcR, int *dimsR){
  
  // Transform data types and allocate intermediate arrays
  ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};
  
  uint8_t *bc = R_Calloc(dims[0] * dims[1], uint8_t);
  
  for (ptrdiff_t idx = 0; idx < dims[0] * dims[1]; idx++) {
    bc[idx] = bcR[idx];
  }
  
  // Call libtopotoolbox function
  fillsinks(output, dem, bc, dims);
  
  // Free the allocated memory
  R_Free(bc);
}