      subroutine wrcrtr(yld,crf,wr,jts,kf)

c  compute weapon radius for etcalc when a crater radius factor (crf)
c  is given.

c  inputs
c        yld    weapon yield (kt)
c        crf    crater radius factor
c        jts    target type
c        kf     k-factor

c  outputs
c        wr     weapon radius (feet)

      include "real8.h"
      include "const.h"

      soilcf = 61.0d0

      if(jts.eq.4.and.kf.eq.9.and.crf.gt.zero)soilcf = 82.0d0

      if(jts.eq.5.and.crf.gt.zero)soilcf = 82.0d0

      if(jts.eq.4.and.kf.lt.9)soilcf = 58.0d0

      if(crf.lt.zero)crf = -crf

      wr = 1.10d0 * crf * soilcf * yld**0.30d0

      return
      end
