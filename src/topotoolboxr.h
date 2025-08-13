#ifndef TOPOTOOLBOXR_H
#define TOPOTOOLBOXR_H

void wrap_has_topotoolbox(int *a);

void wrap_hillshade(float *outputR, float *dxR, float *dyR, float *demR, float *azimuthR, float *altitudeR, float *cellsizeR, int *dimsR);

void wrap_hillshade_fused(float *outputR, float *demR, float *azimuthR, float *altitudeR, float *cellsizeR, int *dimsR);

void wrap_gradient8(float *outputR,float *demR, float *cellsizeR,int *use_mpR, int* dimsR);

void wrap_fillsinks(float *output, float *dem, int *bcR, int *dimsR);

void wrap_fillsinks_hybrid(float *output, float *dem, int *bcR, int *dimsR);

void wrap_gwdt_computecosts(float *costsR, int *flatsR, float *original_demR, float *filled_demR, int *dimsR);

void wrap_gwdt(float *distR, float *costsR, int *flatsR, int *dimsR);

void wrap_flow_routing_d8_carve(int *sourceR, int *directionR, float *demR, float *distR, int *flatsR, int *dimsR);

void wrap_flow_routing_d8_edgelist(int *sourceR, int *targetR, int *nodeR, int *directionR, int *dimsR);

#endif // TOPOTOOLBOXR_H
