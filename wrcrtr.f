      subroutine wrcrtr(yld,crf,wr,jts,kf)
c
c  compute weapon radius for etcalc when a crater radius factor (crf)
c  is given.
c
c  inputs
c        yld    weapon yield (kt)
c        crf    crater radius factor
c        jts    target type
c        kf     k-factor
c
c  outputs
c        wr     weapon radius (feet)
c
      include "real8.h"
      include "const.h"
      soilcf=61.0
      if(jts.eq.4.and.kf.eq.9.and.crf.gt.zero)soilcf=82.0
      if(jts.eq.5.and.crf.gt.zero)soilcf=82.0
      if(jts.eq.4.and.kf.lt.9)soilcf=58.0
      if(crf.lt.zero)crf=-crf
c
      wr=1.1*crf*soilcf*yld**0.30
c
      return
      end
