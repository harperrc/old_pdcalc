      subroutine intgf(z,r,x,f)

      include "real8.h"
      include "const.h"

      rx=r*x
      if(rx.gt.3.75)goto 111
c
      ts=0.071111111*(rx*rx)
      f=(r*exp(-0.50*(r*r+x*x)))*(one+ts*(3.5156229+ts*
     *  (3.0899424+ts*(1.2067492+ts*(0.2659732+ts*(0.0360768+
     *  ts*0.0045813))))))
      if(z.le.3.87)goto 222
      return
c
 111  ti=3.75/rx
      f=0.51639778*r*exp(-0.50*(x-r)**2)*sqrt(ti)*
     *  ((((((((0.00392377*ti-0.01647633)*ti+0.02635537)*ti
     *  -0.02057706)*ti+0.00916281)*ti-0.00157565)*ti
     *  +0.00225319)*ti+0.01328592)*ti+0.39894228)
      if(z.gt.3.87)return
c
 222  sign=one
      if(z.lt.zero)sign=-one
c
      u=0.70710678*abs(z)
      f=f*(0.50+0.50*sign*(one-one/
     *  ((one+u*(0.278393+u*(0.230389+u*(0.000972+u*0.078108))))**4)))
c
      return
      end
