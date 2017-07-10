
#include "dynet.h"

Dim* new_Dim(LongVector ds) { return new Dim(*ds); }
Dim* new_Dim(LongVector ds, int bs) { return new Dim(*ds); }
int Dim_size(Dim* d) { return d->size(); }
