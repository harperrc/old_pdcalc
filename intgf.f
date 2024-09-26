      subroutine intgf(z,r,x,f)

      include "real8.h"
      include "const.h"

      rx = r * x
      if(rx.gt.3.75d0)goto 111

      ts = 0.071111111d0 * (rx * rx)

      f  = (r * exp(-0.50d0 * (r * r + x * x))) * (
     *      one + ts * (3.5156229d0 + ts *
     *     (3.0899424d0 + ts * (1.2067492d0 + ts * (
     *      0.2659732d0 + ts * (0.0360768d0 + ts * 0.0045813d0))))))

      if(z.le.3.87d0)goto 222

      return

 111  ti = 3.75d0 / rx
      f = 0.51639778d0 * r * exp(-0.50d0 * (x - r)**2) * sqrt(ti) *
     *  ((((((((0.00392377d0 * ti - 0.01647633d0 ) * ti + 
     *  0.02635537d0 ) * ti -0.02057706d0) * ti + 0.00916281d0) * ti -
     *  0.00157565d0) * ti + 0.00225319d0) * ti + 0.01328592d0 ) * ti +
     *  0.39894228d0)

      if(z.gt.3.87d0)return

 222  sig = one
      if(z.lt.zero)sig = -one

      u = 0.70710678d0 * abs(z)
      f = f * (0.50d0 + 0.50d0 * sig * (one - one /
     *  ((one + u * (0.278393d0 + u * (0.230389d0 + 
     *  u * (0.000972d0 + u * 0.078108d0))))**4)))

      return
      end
