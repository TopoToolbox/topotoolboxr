#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_hillshade_fused(float *outputR,
                          float *demR,
                          float *azimuthR,
                          float *altitudeR,
                          float *cellsizeR,
                          int *dimsR){

  // Transformation of integer to ptrdiff_t dimensions
  ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};
  
  // Call libtopotoolbox function
  hillshade_fused(outputR,
                  demR,
                  *azimuthR,
                  *altitudeR,
                  *cellsizeR,
                  dims);
}