#!/bin/csh -f
#
# Plot wavelet
#
#
# JS: nz=601, nx=576, ny=351
#
segyread tape=/work53/JS_WIRE/STAWI_01/EffectiveSource_1-30Hz_resamp.segy endian=0 | \
suxwigb perc=99 style=normal wbox=700 hbox=300 title="Wavelet" interp=1 wigclip=1
