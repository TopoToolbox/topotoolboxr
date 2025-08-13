#include <stddef.h>
#include <stdint.h>
#include <R.h>

#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_hillshade(float *outputR,
                    float *dxR,
                    float *dyR,
                    float *demR,
                    float *azimuthR,
                    float *altitudeR,
                    float *cellsizeR,
                    int *dimsR){

  // Transformation of integer to ptrdiff_t dimensions
  ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};

  // Call libtopotoolbox function
  hillshade(outputR,
            dxR,
            dyR,
            demR,
            *azimuthR,
            *altitudeR,
            *cellsizeR,
            dims);
}