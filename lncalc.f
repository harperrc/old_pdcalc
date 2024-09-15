      subroutine lncalc(cep,dsig,wr,r95,pod,d,iflg,ierr)
c
c  compute the probability of damage or calculate the offset distance
c  to acheive a specific probability of damage.
c
c  inputs:
c      cep    circular error probable (feet)
c      dsig   damage sigma
c      wr     weapon radius (feet)
c      r95    95 % radius
c
c  inputs/outputs
c      pod    probability of damage
c      d      distance from dgz to target (nmi)
c      iflg   options flag
c             = 1 limit pod <=0.99
c             = 2 limit pod <=0.999
c             = 6 set d = zero
c      ierr   error status
c
      include "real8.h"
      logical cross
      include "const.h"
      include "files.h"

      dimension w(5),zp(5)

      data w/0.0666713443d0,0.1494513492d0,0.2190863625d0,
     *       0.2692667193d0,0.2955242247d0/

      data zp/0.9739065285d0,0.8650633667d0,0.6794095683d0,
     *        0.4333953941d0,0.1488743390d0/

      ierr=0

      if(iflg.eq.6)d = zero

      d    = d * cnm2ft
      itch = 0

      rr5 = r95 * cnm2ft

      adcep = sqrt(cep * cep + 0.231d0 * rr5 * rr5)
c
      if(wr.le.0.001)goto 40

c  compute beta-factor used in computing z

 10   ex   = one - dsig * dsig
      beta = sqrt(-log(ex))
c
      if(adcep.gt.zero)goto 50
c
c  compute pod when cep=r95=zero
c
c  if d = zero set pod = 0.999
c
      if(d.le.zero)goto 20
c
      z=(one/beta)*log(wr*ex/d)
c
c  if z >3.87 pod = 0.999.  if z is close to zero pod = 0.50
c  if z < -3.87 pod = zero
c
      if(z.gt.3.87)goto 20
      zab=abs(z)
      if(zab.lt.5.0e-7)goto 30
      if(z.lt.-3.87)goto 40
c
c  pod = 0.50+0.50*abs(z)/z*erf(1)
c
      c=abs(z)/1.414213562
c
      c2=c*c
      c3=c2*c
      c4=c3*c
      c5=c4*c
      c6=c5*c
c
      erfu=one-one/((one+0.0705230784*c+0.0422820123*c2+
     *  0.0092705272*c3+0.0001520143*c4+0.0002765672*c5+
     *  0.0000430638*c6)**16)
c
      if(z.lt.zero) then
         pov=0.50-0.50*erfu
      else
         pov=0.50+0.50*erfu
      endif
c
      goto 120
c
 20   pov=0.999
      goto 120
c
 30   pov=0.500
      goto 130
c
 40   pov=zero
      goto 130
c
c  normalize wr and d
c
 50   wrn=1.1774*wr/adcep
      x  =1.1774*d/adcep
c
c  fsum will sum terms of gaussian quadrature
c
      fsum=zero
c
c  if dn - 4 < 0 begin integration with radius of zero otherwise at dt-4
c
      xbb=1.06*wrn*exp(2.86*dsig)
c
      xb=x+4.0
c
      if(xbb.lt.xb)xb=xbb
c
      if(x.le.4.0) then
         xa=zero
         bplusa=xb
         bminsa=xb
      else
         xa=x-4.0
         bplusa=xa+xb
         bminsa=xb-xa
         if(bminsa.le.zero)goto 110
      endif
c
      wrnx  = wrn * ex
      betai = one / beta
c
      do 100 n=1,5
c
      r1=(-bminsa*zp(n)+bplusa)/2.0
      r2=( bminsa*zp(n)+bplusa)/2.0
c
      z1=betai*log(wrnx/r1)
      z2=betai*log(wrnx/r2)
c
      call intgf(z1,r1,x,f)
      fsum=fsum+w(n)*f
      if(z2.lt.-3.87)goto 100
      call intgf(z2,r2,x,f)
      fsum=fsum+w(n)*f
 100  continue
 110  continue
c
      pov=0.50*fsum*bminsa
c
 120  if(iflg.eq.6)goto 140
      if(pov.le.0.99)goto 130
      if(iflg.eq.1)pov=0.99
      if(pov.gt.0.999)pov=0.999
c
 130  pod=pov
c
      d=d/cnm2ft
c
      return
c
c  this is where computation of d, offset distance, occurs if it is
c  desired.  this computes the max distance at which a given minimum
c  pod can be obtained
c
 140  if(itch.gt.0)goto 150
      if(pov.lt.pod)goto 180
c
      itch=1
c
      acc=0.001
      cross=.false.
      dd=wr
      d=wr
      goto 10
c
 150  pda=abs(pod-pov)
      if(pda.lt.acc)goto 170
      if(pod.gt.pov)goto 160
      if(cross)dd=dd/2.0
      d=d+dd
      goto 10
c
 160  cross=.true.
      dd=dd/2.0
      d=d-dd
      goto 10
c
 170  d=d/cnm2ft
      return
c
 180  ierr=1
      d=zero
c
      return
      end
