      subroutine acon

      include "real8.h"
      include "const.h"
      include "files.h"

      namelist /conlst/ re,we,g0,ttol
c
c  initialize data
c
      zero       = 0.0d0
      one        = 1.0d0
      pi         = 4.0d0 * atan(one)
      twopi      = 2.0d0 * pi
      forpi3     = (4.0d0 * pi)**3
      forpi2     = (4.0d0 * pi)**2
      pi2thd     = 2.0d0 * pi / 3.0d0
      piovr4     = pi / 4.0d0
      third      = one / 3.0d0
      pi886      = pi * 0.886d0
      pi2ov4     = pi * pi / 4.0d0
      pi2o16     = pi * pi / 16.0d0
      trpio4     = 3.0d0 * pi / 4.0d0
      pi4th      = pi**4
      dpr        = 180.0d0 / pi
      piovr2     = pi / 2.0d0
      cmf        = 30.48d0
      ckm2ft     = 100000.0d0 / 30.48d0
      cm2ft      = 100.0d0 / 30.48d0
      ckm2ft2    = ckm2ft * ckm2ft
      cnm2ft     = 6076.115d0
      bk         = 1.38d-23
      cghz2ft    = 2.997d8 * 3.2808d0 / 1.0d9
      small      = 1.0d-8
      cft2cm2    = 30.48d0 * 30.48d0
      xmterg     = 4.186d22
      am         = 2.5d-23
      ei         = 2.24d-11
      ohnft      = 100.0d0 * ckm2ft
      re         = 20902267.0d0
      we         = 7.29211585d-5
      g0         = 32.174d0
      ttol       = 1.0d-4
      half       = 0.50d0
      sqr2pi     = sqrt(2.0d0 * pi)
      spdlig     = 2.99792458d8
c
cccc      read(lin,conlst)
cccc      write(lout,conlst)
c
      twore      = 2.0d0 *re
      re2        = re *re
      rekm       = re / ckm2ft
      rkp100     = rekm + 100.0d0
      rekp120    = rekm + 120.0d0
      rftp100    = rkp100 * ckm2ft
      rkp1002    = rkp100 * rkp100
      xmu        = g0 * re2
      twoxmu     = 2.0d0 * xmu
      xmusqr     = sqrt(xmu)
      cx0        = 6.4d0 * ckm2ft * ckm2ft
      recm       = re * 30.48d0
      grav       = g0 * 30.48d0
c
      we4   = sngl(we)
      grav4 = sngl(grav)
      recm4 = sngl(recm)
c
      return
      end
