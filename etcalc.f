      subroutine etcalc(iv,jt,kf,yld,cep,hob1,orien,azmth,di,pod,wr,
     *  ierr)

c  compute probability of damage for equivalent target area (eta) type 
c  targets
c
c  inputs
c        iv     vulnerability number
c        jt     target type
c        kf     k-factor
c        yld    weapon yield (kt)
c        cep    circular error probable (feet)
c        hob1   height of burst (feet)
c        orien  
c        azmth
c
c  inputs/outputs
c        d      distance from dgz to target
c        pod    probability of damage
c        wr     weapon radius
c        ierr   error status


      include "real8.h"
      include "const.h"
      include "files.h"

      dimension inw(3,10,6),crw(10,6),dswv(10,6),vnw(10,6),
     *  inl(6,10,6),crl(2,10,6),dslv(2,10,6),vnl(10,6)

      dd(b,c) = abs(b) / (1.414213562d0 * c)

      er(b,c) =  one + dd(b,c) * 
     *          (w1 + dd(b,c) * (w2 + dd(b,c) * (w3 + dd(b,c) *
     *          (w4 + dd(b,c) * (w5 + dd(b,c) * w6)))))

      erfp(b,c) = (one - (one / er(b,c))**16) * abs(b) / (2.0d0 * b)

      p(b,c,d,e,f,g,h,a) = (erfp(d,e) - erfp(b,c)) * 
     *                     (erfp(h,a) - erfp(f,g))

      acep(a,b) = sqrt(cep * cep + (1.1774d0 * a * b)**2) / 1.1774d0

      data inw/

c  bridges

     *  0,0,0,  0,0,0,  0,0,0, 31,1,0, 25,2,6, 20,2,6, 18,2,6, 25,2,8,
     * 15,2,9, 16,2,8,  0,0,0, 18,2,9, 17,2,9, 16,2,8, 15,2,9, 17,2,8,
     * 14,2,9, 16,2,9, 16,2,9,  0,0,0, 
     * 18,2,9, 17,2,9, 16,2,8, 15,2,9, 16,2,9, 17,2,8, 17,2,8, 9*0,

c  dams (upstream vntk)

     * 41,1,0, 38,1,0, 38,1,0, 42,1,0, 39,1,0, 39,1,0, 39,1,0, 35,1,0,
     * 35,1,0,  0,0,0,

c  locks

     *  30*0,

c  special case

     *  0,0,0, 13,2,5, 11,2,4, 21*0/
c
      data crw/

c  bridges

     *  1.5d0,2.0d0,1.5d0,27*0.0d0,

c  dams (upstream)

     *  9*0.0d0,1.0d0,

c  locks

     *  1.0d0,1.5d0,1.0d0,1.5d0,1.0d0,1.5d0,4*0.d0,

c  special case

     *  10*0.d0/
c
      data inl/

c  bridges

     * 18*0,       38,1,4*0,   29,2,6,3*0, 23,2,6,3*0, 21,2,6,3*0,
     * 29,2,8,3*0, 18,2,9,3*0, 22,2,8,9*0, 22,2,9,3*0, 20,2,9,3*0,
     * 19,2,8,3*0, 21,2,7,3*0, 23,2,8,3*0, 23,2,7,3*0, 25,2,8,3*0,
     * 24,2,8,9*0, 22,2,8,3*0, 22,2,8,3*0, 22,2,8,3*0, 23,2,7,3*0,
     * 25,2,8,3*0, 23,2,7,3*0, 25,2,8,21*0,

c  dams (downstream vntk)

     *  60*0,

c  locks

     * 12*0, 31,1,4*0, 31,1,4*0, 31,1,0, 31,1,0, 31,1,0, 31,1,25*0,

c  special case

     * 6*0, 13,2,5, 3*0, 11,2,4, 45*0/

      data crl/

c  bridges

     * 1.25d0,0.d0,1.5d0,0.d0,1.25d0,0.d0,34*0.d0,
     * 20*0.d0,

c  dams (downstream crf)

     * 0.5d0, 0.d0, 0.5d0, 0.d0, 0.5d0, 0.d0, 0.5d0, 0.d0, 
     * 0.5d0, 0.d0, 0.5d0, 0.d0, 0.5d0, 0.d0, 0.5d0, 0.d0, 
     * 0.5d0, 0.d0, 1.5d0, 0.d0, 

c  locks

     * 2*1.0d0,2*1.50d0,0.00d0,1.00d0,0.00d0,1.50d0,12*0.0d0,

c  special case

     *  20*0.0d0/

      data dswv/

c  bridges

     *  3*0.3d0,0.2d0,6*0.3d0,0.0d0,8*0.3d0,0.0d0,
     *  7*0.3d0,3*0.d0,

c  dams (upstream dsig)

     *  9*0.2d0,0.3d0,

c  locks

     *  6*0.3d0,4*0.d0,

c  special case

     *  0.0d0,0.3d0,0.3d0,7*0.0d0/

      data dslv/

c  brigdes

     * 0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.20d0,
     * 0.00d0 ,0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.30d0 ,0.00d0,
     * 0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.00d0,
     * 0.00d0 ,0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.30d0 ,0.00d0,
     * 0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.30d0,
     * 0.00d0 ,0.30d0 ,0.00d0 ,0.00d0 ,0.00d0 ,0.30d0 ,0.00d0,
     * 0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.30d0,
     * 0.00d0 ,0.30d0 ,0.00d0 ,0.30d0 ,0.00d0 ,0.00d0 ,0.00d0,
     * 0.00d0 ,0.00d0 ,0.00d0 ,0.00d0,

c  dams (downstream dsig)

     * 0.0d0 ,0.30d0 ,0.0d0 ,0.30d0 ,0.0d0 ,0.30d0 ,0.0d0 ,0.30d0,
     * 0.0d0 ,0.30d0 ,0.0d0 ,0.30d0 ,0.0d0 ,0.30d0 ,0.0d0 ,0.30d0,
     * 0.0d0 ,0.30d0 ,0.0d0 ,0.30d0,

c  locks

     * 4*0.30d0,0.20d0,0.30d0,0.20d0,0.30d0,4*0.20d0,8*0.0d0,

c  special case

     *  2*0.0d0 ,0.30d0 ,0.0d0 ,0.30d0 ,0.0d0 ,14*0.0d0/

      data vnw/

c  bridges

     *  5.0d0 ,15.0d0 ,25.0d0 ,35.0d0 ,45.0d0 ,55.0d0 ,65.0d0,
     * 75.0d0 ,85.0d0 ,90.0d0 , 5.0d0 ,15.0d0 ,25.0d0 ,35.0d0,
     * 45.0d0 ,55.0d0 ,65.0d0 ,75.0d0 ,85.0d0 ,90.0d0 , 5.0d0,
     * 15.0d0 ,25.0d0 ,35.0d0 ,45.0d0 ,55.0d0 ,65.0d0 ,75.0d0,
     * 85.0d0 ,90.0d0,

c  dams

     *   5.0d0 , 15.0d0 , 26.0d0 ,40.0d0 ,57.0d0 ,82.0d0 ,
     * 114.0d0 ,163.0d0 ,229.0d0 ,262.0d0,

c  locks

     *  33.0d0 , 40.0d0 , 60.0d0 , 75.0d0 , 90.0d0 ,110.0d0 ,
     * 125.0d0 ,145.0d0 ,180.0d0 ,200.0d0 ,

c  special case

     *  2000.0d0 ,1900.0d0 ,1700.0d0 ,1500.0d0 ,1300.0d0 ,
     *  1100.0d0 , 900.0d0 , 700.0d0 , 500.0d0 , 300.0d0/

      data vnl/

c  bridges

     *   50.0d0 , 150.0d0 , 400.0d0 , 800.0d0 ,1200.0d0 ,1600.0d0,
     * 2000.0d0 ,2400.0d0 ,2800.0d0 ,3000.0d0 ,  50.0d0 , 150.0d0,
     *  400.0d0 , 800.0d0 ,1200.0d0 ,1600.0d0 ,2000.0d0 ,2400.0d0,
     * 2800.0d0 ,3000.0d0 ,  50.0d0 , 150.0d0 , 400.0d0 , 800.0d0,
     * 1200.0d0 ,1600.0d0 ,2000.0d0 ,2400.0d0 ,2800.0d0 ,3000.0d0,

c  dams

     *  500.0d0 ,  750.0d0 , 1500.0d0 , 2500.0d0 ,3500.0d0 ,4500.0d0,
     * 7500.0d0 ,12500.0d0 ,20000.0d0 ,25750.0d0,

c  locks

     *   98.0d0 , 130.0d0 , 250.0d0 , 500.0d0 ,800.0d0 ,1300.0d0,
     * 2000.0d0 ,2450.0d0 ,2800.0d0 ,3000.0d0,

c  special case

     *  10000.0d0 ,9500.0d0 ,8500.0d0 ,7500.0d0 ,6500.0d0 ,
     *   5500.0d0 ,4500.0d0 ,3500.0d0 ,2500.0d0 ,2000.0d0/

      data w1,w2,w3,w4,w5,w6/
     *  0.0705230784d0, 0.0422820123d0, 0.0092705272d0, 
     *  0.0001520143d0, 0.0002765672d0, 0.0000430638d0/

      ierr = 0

      igv = iv / 10
      ign = iv - igv * 10

      wrl1 = zero
      wrl2 = zero
      wrw1 = zero

      kk = kf + 1

      if(ign.eq.0)ign = 10
      if(igv.eq.0)igv = 10

      jts = jt
      goto (100,110,110,300,200,400),jts

      write(lout,*)' goto error in etcalc ',jts
      stop' got error'

c  jts to 1 or 2 or 3 for bridges
c
c  if air-burst for a0,a1 or a2 type bridges set pod to zero

 100  if(kf.lt.3.and.hob1.gt.0.99d0)goto 500

c  determine weapon radii
c
c  see if crater or non-crater

 110  if(crl(1,kk,jts).gt.zero) then
         call wrcrtr(yld,crl(1,kk,jts),wrl1,jts,kf)
      endif

      if(inl(2,kk,jts).gt.0) then
         call wrcalc(yld,hob1,inl(1,kk,jts),inl(2,kk,jts),
     *      inl(3,kk,jts),dslv(1,kk,jts),wrl1,ierr)
      endif

      if(crw(kk,jts).gt.zero) then
         call wrcrtr(yld,crw(kk,jts),wrw1,jts,kf)
      endif

      if(inw(2,kk,jts).gt.0) then
         call wrcalc(yld,hob1,inw(1,kk,jts),inw(2,kk,jts),
     *      inw(3,kk,jts),dswv(kk,jts),wrw1,ierr)
      endif

c  determine x and y offset distances
c  orien is the target direction
c  azmth is the azimuth from dgz to target
c  xo is the east-west component
c  yo is the north-south component

      ddum  = di * cnm2ft
      angle = (azmth - orien * 10.0d0) / dpr
      xo    = ddum * sin(angle)
      yo    = ddum * cos(angle)

c  compute boundaries

      w  = vnw(ign,jts)
      sl = vnl(igv,jts)

      a  =  -w /2.0d0 - wrw1 + xo
      b  =   w /2.0d0 + wrw1 + xo
      c  = -sl /2.0d0 - wrl1 + yo
      d  =  sl /2.0d0 + wrl1 + yo

c  compute delivery sigmas

      aa = acep(wrw1,dswv(kk,jts))
      ab = aa
      ac = acep(wrl1,dslv(1,kk,jts))
      ad = ac

c  compute pod

      pod = p(a,aa,b,ab,c,ac,d,ad)
      return

c  lock section
c
c  if air-burst set pod to zero

 200  if(hob1.gt.0.001d0)goto 500

c   determine weapon radii

      if(crl(1,kk,jts).gt.zero) then
         call wrcrtr(yld,crl(1,kk,jts),wrl1,jts,kf)
      endif

      if(inl(2,kk,jts).gt.0) then
         call wrcalc(yld,hob1,inl(1,kk,jts),inl(2,kk,jts),
     *      inl(3,kk,jts),dslv(1,kk,jts),wrl1,ierr)
      endif

      if(crl(2,kk,jts).gt.zero) then
         call wrcrtr(yld,crl(2,kk,jts),wrl2,jts,kf)
      endif

      if(inl(5,kk,jts).gt.0) then
         call wrcalc(yld,hob1,inl(4,kk,jts),inl(5,kk,jts),
     *      inl(6,kk,jts),dslv(1,kk,jts),wrl2,ierr)
      endif

      if(crw(kk,jts).gt.zero)crw(kk,jts)=-crw(kk,jts)

      if(crw(kk,jts).lt.zero) then
         call wrcrtr(yld,crw(kk,jts),wrw1,jts,kf)
      endif

      if(inw(2,kk,jts).gt.0) then
         call wrcalc(yld,hob1,inw(1,kk,jts),inw(2,kk,jts),
     *     inw(3,kk,jts),dswv(kk,jts),wrw1,ierr)
      endif

      wr = (wrl2 - wrl1) / 2.0d0
      if(inl(2,kk,jts).gt.0)wr = (wrl1 - wrl2) / 2.0d0

c  determine x and y offset distances
c  orien is the target direction
c  azmth is the azimuth from dgz to target
c  xo is the east-west component
c  yo is the north-south component

      ddum  = di * cnm2ft
      angle = (azmth - orien * 10.0d0) / dpr
      xo    = ddum * sin(angle)
      yo    = ddum * cos(angle)

c  compute boundaries

      w  = vnw(ign,jts)
      sl = vnl(igv,jts)

      a  = -w / 2.0d0 - wrw1 + xo
      b  =  w / 2.0d0 + wrw1 + xo

      aa = acep(wrw1,dswv(kk,jts))
      ab = aa

      if(inl(2,kk,jts).gt.0)goto 210

      c  = -sl / 2.0d0 - wrl1 + yo
      d  =  sl / 2.0d0 + wrl2 + yo

      ac = acep(wrl1,dslv(1,kk,jts))
      ad = acep(wrl2,dslv(2,kk,jts))
      goto 220

 210  c   = -sl / 2.0d0 - wrl2 + yo
      d   =  sl / 2.0d0 + wrl1 + yo
      ac  =  acep(wrl2,dslv(2,kk,jts))
      ad  =  acep(wrl1,dslv(1,kk,jts))

 220  poc = p(a,aa,b,ab,c,ac,d,ad)
      return

c  dam section
c
c  jts = 4 for dams

 300  if(hob1.gt.0.001d0)goto 500

c  determine weapon radii

      if(crl(1,kk,jts).gt.zero)crl(1,kk,jts)=-crl(1,kk,jts)

      if(crl(1,kk,jts).lt.zero) then
         call wrcrtr(yld,crl(1,kk,jts),wrl1,jts,kf)
      endif

      if(inl(2,kk,jts).gt.0) then
         call wrcalc(yld,hob1,inl(1,kk,jts),inl(2,kk,jts),
     *      inl(3,kk,jts),dslv(1,kk,jts),wrl1,ierr)
      endif

      if(crw(kk,jts).gt.zero) then
         call wrcrtr(yld,crw(kk,jts),wrw1,jts,kf)
      endif

      if(inw(2,kk,jts).gt.0) then
         call wrcalc(yld,hob1,inw(1,kk,jts),inw(2,kk,jts),
     *      inw(3,kk,jts),dswv(kk,jts),wrw1,ierr)
      endif

      wr = (wrw1 - wrl1) / 2.0d0

c  determine x and y offset distances
c  orien is the target direction
c  azmth is the azimuth from dgz to target
c  xo is the east-west component
c  yo is the north-south component

      ddum  = di * cnm2ft
      angle = (azmth - orien * 10.0d0) / dpr
      xo    = ddum * sin(angle)
      yo    = ddum * cos(angle)

c  compute boundaries

      w  = vnw(ign,jts)
      sl = vnl(igv,jts)

      c  =-sl / 2.0d0 + yo
      d  = sl / 2.0d0 + yo
      if(kf.eq.9)goto 310

      a  = -wrw1 - 0.10d0 + xo
      b  =  wrl1 - 0.10d0 + xo
      goto 320

 310  a   = -wrw1 + w / 2.0d0 + xo
      b   =  wrl1 - w / 2.0d0 + xo
c
 320  aa  = acep(wrw1,dswv(kk,jts))
      ab  = acep(wrl1,dslv(2,kk,jts))
      ac  = acep(sl/2.0,dslv(1,kk,jts))
      ad  = ac

      pod = p(a,aa,b,ab,c,ac,d,ad)

      if(pod.lt.zero)goto 500
      return

c  special case section

 400  call wrcalc(yld,hob1,inl(1,kk,jts),inl(2,kk,jts),
     *  inl(3,kk,jts),dslv(1,kk,jts),wrl1,ierr)

      wrw1 = wrl1

c  determine x and y offset distances
c  orien is the target direction
c  azmth is the azimuth from dgz to target
c  xo is the east-west component
c  yo is the north-south component

      ddum  = di * cnm2ft
      angle = (azmth - orien * 10.0d0) / dpr
      xo    = ddum * sin(angle)
      yo    = ddum * cos(angle)

c  compute boundaries

      w   = vnw(ign,jts)
      sl  = vnl(igv,jts)

      a   =  -w / 2.0d0 - wrw1 + xo
      b   =   w / 2.0d0 + wrw1 + xo
      c   = -sl / 2.0d0 - wrl1 + yo
      d   =  sl / 2.0d0 + wrl1 + yo

      aa  = acep(wrw1,dswv(kk,jts))
      ab  = aa
      ac  = acep(wrw1,dslv(1,kk,jts))
      ad  = ac

      pod = p(a,aa,b,ab,c,ac,d,ad)
      return

 500  pod = zero

      return
      end
