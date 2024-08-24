      subroutine acon
      include "real8.h"
      include "const.h"
      include "files.h"
      namelist /conlst/ re,we,g0,ttol
c
c  initialize data
c
      zero       = 0.0
      one        = 1.0
      pi         = 4.0*atan(one)
      twopi      = 2.0*pi
      forpi3     = (4.0*pi)**3
      forpi2     = (4.0*pi)**2
      pi2thd     = 2.0*pi/3.0
      piovr4     = pi/4.0
      third      = one/3.0
      pi886      = pi*0.886
      pi2ov4     = pi*pi/4.0
      pi2o16     = pi*pi/16.0
      trpio4     = 3.0*pi/4.0
      pi4th      = pi**4
      dpr        = 180./pi
      piovr2     = pi/2.0
      cmf        = 30.48
      cnm2ft     = 6076.115
      ckm2ft     = 100000.0/30.48
      cm2ft      = 100.0/30.48
      ckm2ft2    = ckm2ft*ckm2ft
      bk         = 1.38e-23
      cghz2ft    = 2.997e8*3.2808/1.0e9
      small      = 1.0e-8
      cft2cm2    = 30.48*30.48
      xmterg     = 4.186e22
      am         = 2.5e-23
      ei         = 2.24e-11
      ohnft      = 100.0*ckm2ft
      re         = 20902267.0
      we         = 7.29211585e-5
      g0         = 32.174
      ttol       = 1.0e-4
      half       = 0.50
      sqr2pi     = sqrt(2.0*pi)
      spdlig     = 2.99792458e8
c
      read(lin,conlst)
      write(lout,conlst)
c
      twore      = 2.0*re
      re2        = re*re
      rekm       = re/ckm2ft
      rkp100     = rekm+100.0
      rekp120    = rekm+120.0
      rftp100    = rkp100*ckm2ft
      rkp1002    = rkp100*rkp100
      xmu        = g0*re2
      twoxmu     = 2.0*xmu
      xmusqr     = sqrt(xmu)
      cx0        = 6.4*ckm2ft*ckm2ft
      recm       = re*30.48
      grav       = g0*30.48
c
      we4=we
      grav4=grav
      recm4=recm
c
      return
      end
