#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_flow_routing_d8_carve(int *sourceR,
                                int *directionR,
                                float *demR,
                                float *distR,
                                int *flatsR,
                                int *dimsR){
  
  // Transformation of data types
  ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};
  ptrdiff_t *source = R_Calloc(dims[0] * dims[1], ptrdiff_t);
  uint8_t *direction = R_Calloc(dims[0] * dims[1], uint8_t);
  
  // Row-major ordering
  unsigned int order = 1;
  
  // Flow routing computation using libtopotoolbox
  flow_routing_d8_carve(source, direction, demR, distR, flatsR, dims, order);
  
  // Write source and direction into arrays passed back to R
  for (ptrdiff_t idx = 0; idx < dims[0] * dims[1]; idx++) {
    sourceR[idx] = (int)source[idx];
    directionR[idx] = (int)direction[idx];
  }
  
  // Free memory
  R_Free(source);
  R_Free(direction);
}