#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_flow_routing_d8_edgelist(int *sourceR, // output
                                   int *targetR, // output
                                   int *nodeR, // input
                                   int *directionR, // input
                                   int *dimsR){ // input
  
  // Transformation of data types and array allocation
  ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};
  ptrdiff_t *source = R_Calloc(dims[0] * dims[1], ptrdiff_t);
  ptrdiff_t *target = R_Calloc(dims[0] * dims[1], ptrdiff_t);
  ptrdiff_t *node = R_Calloc(dims[0] * dims[1], ptrdiff_t);
  uint8_t *direction = R_Calloc(dims[0] * dims[1], uint8_t);

  for (ptrdiff_t idx = 0; idx < dims[0] * dims[1]; idx++) {
    node[idx] = (ptrdiff_t)nodeR[idx];
    direction[idx] = (uint8_t)directionR[idx];
  }
  
  // Row-major ordering due to usage of terra
  unsigned int order = 1;
  
  // Flow routing computation using libtopotoolbox
  flow_routing_d8_edgelist(source, target, node, direction, dims, order);
  
  // Write source and target into arrays passed back to R
  for (ptrdiff_t idx = 0; idx < dims[0] * dims[1]; idx++) {
    sourceR[idx] = (int)source[idx];
    targetR[idx] = (int)target[idx];
  }
  
  // Free memory
  R_Free(source);
  R_Free(target);
  R_Free(node);
  R_Free(direction);
}