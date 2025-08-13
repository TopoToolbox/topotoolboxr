#ifndef TOPOTOOLBOXR_H
#define TOPOTOOLBOXR_H

void wrap_has_topotoolbox(int *a);

void wrap_gradient8(float *outputR,float *demR, float *cellsizeR,int *use_mpR, int* dimsR);

void wrap_fillsink(float *output, float *dem, int *bcR, int *dimsR);

void wrap_gwdt_computecosts(float *costsR, int *flatsR, float *original_demR, float *filled_demR, int *dimsR);

void wrap_gwdt(float *distR, float *costsR, int *flatsR, int *dimsR);

void wrap_flow_routing_d8_carve(int *sourceR, int *directionR, float *demR, float *distR, int *flatsR, int *dimsR);

void wrap_flow_routing_d8_edgelist(int *sourceR, int *targetR, int *nodeR, int *directionR, int *dimsR);

#endif // TOPOTOOLBOXR_H
