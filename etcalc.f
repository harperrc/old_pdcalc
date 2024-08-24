      subroutine etcalc(iv,jt,kf,yld,cep,hob1,orien,azmth,di,pod,wr,
     *  ierr)
c
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
c
      include "real8.h"
      include "const.h"
      include "files.h"
      dimension inw(3,10,6),crw(10,6),dswv(10,6),vnw(10,6),
     *  inl(6,10,6),crl(2,10,6),dslv(2,10,6),vnl(10,6)
c
      dd(b,c)=abs(b)/(1.414213562*c)
c
      er(b,c)=one+dd(b,c)*(w1+dd(b,c)*(w2+dd(b,c)*(w3+dd(b,c)*
     *  (w4+dd(b,c)*(w5+dd(b,c)*w6)))))
c
      erfp(b,c)=(one-(one/er(b,c))**16)*abs(b)/(2.0*b)
c
      p(b,c,d,e,f,g,h,a)=(erfp(d,e)-erfp(b,c))*(erfp(h,a)-erfp(f,g))
c
      acep(a,b)=sqrt(cep*cep+(1.1774*a*b)**2)/1.1774
c
      data inw/
c
c  bridges
c
     *  0,0,0,  0,0,0,  0,0,0, 31,1,0, 25,2,6, 20,2,6, 18,2,6, 25,2,8,
     * 15,2,9, 16,2,8,  0,0,0, 18,2,9, 17,2,9, 16,2,8, 15,2,9, 17,2,8,
     * 14,2,9, 16,2,9, 16,2,9,  0,0,0, 
     * 18,2,9, 17,2,9, 16,2,8, 15,2,9, 16,2,9, 17,2,8, 17,2,8, 9*0,
c
c  dams (upstream vntk)
c
     * 41,1,0, 38,1,0, 38,1,0, 42,1,0, 39,1,0, 39,1,0, 39,1,0, 35,1,0,
     * 35,1,0,  0,0,0,
c
c  locks
c
     *  30*0,
c
c  special case
c
     *  0,0,0, 13,2,5, 11,2,4, 21*0/
c
      data crw/
c
c  bridges
c
     *  1.5,2.0,1.5,27*0.,
c
c  dams (upstream)
c
     *  9*0.,1.0,
c
c  locks
c
     *  1.0,1.5,1.0,1.5,1.0,1.5,4*0.,
c
c  special case
c
     *  10*0./
c
      data inl/
c
c  bridges
c
     * 18*0,       38,1,4*0,   29,2,6,3*0, 23,2,6,3*0, 21,2,6,3*0,
     * 29,2,8,3*0, 18,2,9,3*0, 22,2,8,9*0, 22,2,9,3*0, 20,2,9,3*0,
     * 19,2,8,3*0, 21,2,7,3*0, 23,2,8,3*0, 23,2,7,3*0, 25,2,8,3*0,
     * 24,2,8,9*0, 22,2,8,3*0, 22,2,8,3*0, 22,2,8,3*0, 23,2,7,3*0,
     * 25,2,8,3*0, 23,2,7,3*0, 25,2,8,21*0,
c
c  dams (downstream vntk)
c
     *  60*0,
c
c  locks
c
     * 12*0, 31,1,4*0, 31,1,4*0, 31,1,0, 31,1,0, 31,1,0, 31,1,25*0,
c
c  special case
c
     * 6*0, 13,2,5, 3*0, 11,2,4, 45*0/
c
      data crl/
c
c  bridges
c
     * 1.25,0.,1.5,0.,1.25,0.,34*0.,
     * 20*0.,
c
c  dams (downstream crf)
c
     * 0.5,0.,0.5,0.,0.5,0.,0.5,0.,0.5,0.,0.5,0.,0.5,0.,0.5,0.,0.5,0.,
     * 1.5,0.,
c
c  locks
c
     * 2*1.,2*1.5,0.0,1.0,0.0,1.5,12*0.,
c
c  special case
c
     *  20*0./
c
      data dswv/
c
c  bridges
c
     *  3*0.3,0.2,6*0.3,0.0,8*0.3,0.0,
     *  7*0.3,3*0.,
c
c  dams (upstream dsig)
c
     *  9*0.2,0.3,
c
c  locks
c
     *  6*0.3,4*0.,
c
c  special case
c
     *  0.0,0.3,0.3,7*0./
c
      data dslv/
c
c  brigdes
c
     * .3,0.,.3,0.,.3,0.,.2,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,
     * 0.,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,0.,0.,
     * .3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,7*0.,
c
c  dams (downstream dsig)
c
     * 0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,0.,.3,
c
c  locks
c
     * 4*0.3,0.2,0.3,0.2,0.3,4*0.2,8*0.,
c
c  special case
c
     *  2*0.,0.3,0.,0.3,0.,14*0./
c
      data vnw/
c
c  bridges
c
     *  5.,15.,25.,35.,45.,55.,65.,75.,85.,90.,
     *  5.,15.,25.,35.,45.,55.,65.,75.,85.,90.,
     *  5.,15.,25.,35.,45.,55.,65.,75.,85.,90.,
c
c  dams
c
     *  5.,15.,26.,40.,57.,82.,114.,163.,229.,262.,
c
c  locks
c
     *  33.,40.,60.,75.,90.,110.,125.,145.,180.,200.,
c
c  special case
c
     *  2000.,1900.,1700.,1500.,1300.,1100.,900.,700.,500.,300./
c
      data vnl/
c
c  bridges
c
     *  50.,150.,400.,800.,1200.,1600.,2000.,2400.,2800.,3000.,
     *  50.,150.,400.,800.,1200.,1600.,2000.,2400.,2800.,3000.,
     *  50.,150.,400.,800.,1200.,1600.,2000.,2400.,2800.,3000.,
c
c  dams
c
     *  500.,750.,1500.,2500.,3500.,4500.,7500.,12500.,20000.,25750.,
c
c  locks
c
     *  98.,130.,250.,500.,800.,1300.,2000.,2450.,2800.,3000.,
c
c  special case
c
     *  10000.,9500.,8500.,7500.,6500.,5500.,4500.,3500.,2500.,2000./
c
      data w1,w2,w3,w4,w5,w6/
     *  0.0705230784,0.0422820123,0.0092705272,0.0001520143,
     *  0.0002765672,0.0000430638/
c
      ierr=0
c
      igv=iv/10
      ign=iv-igv*10
c
      wrl1=zero
      wrl2=zero
      wrw1=zero
c
      kk=kf+1
c
      if(ign.eq.0)ign=10
      if(igv.eq.0)igv=10
c
      jts=jt
      goto (100,110,110,300,200,400),jts
      write(lout,*)' goto error in etcalc ',jts
      stop' got error'
c
c  jts to 1 or 2 or 3 for bridges
c
c  if air-burst for a0,a1 or a2 type bridges set pod to zero
c
 100  if(kf.lt.3.and.hob1.gt.0.99)goto 500
c
c  determine weapon radii
c
c  see if crater or non-crater
c
 110  if(crl(1,kk,jts).gt.zero) then
         call wrcrtr(yld,crl(1,kk,jts),wrl1,jts,kf)
      endif
      if(inl(2,kk,jts).gt.0) then
         call wrcalc(yld,hob1,inl(1,kk,jts),inl(2,kk,jts),
     *      inl(3,kk,jts),dslv(1,kk,jts),wrl1,ierr)
      endif
c
      if(crw(kk,jts).gt.zero) then
         call wrcrtr(yld,crw(kk,jts),wrw1,jts,kf)
      endif
      if(inw(2,kk,jts).gt.0) then
         call wrcalc(yld,hob1,inw(1,kk,jts),inw(2,kk,jts),
     *      inw(3,kk,jts),dswv(kk,jts),wrw1,ierr)
      endif
c
c  determine x and y offset distances
c  orien is the target direction
c  azmth is the azimuth from dgz to target
c  xo is the east-west component
c  yo is the north-south component
c
      ddum=di*cnm2ft
      angle=(azmth-orien*10.0)/dpr
      xo=ddum*sin(angle)
      yo=ddum*cos(angle)
c
c  compute boundaries
c
      w=vnw(ign,jts)
      sl=vnl(igv,jts)
c
      a=-w/2.0-wrw1+xo
      b= w/2.0+wrw1+xo
      c=-sl/2.0-wrl1+yo
      d= sl/2.0+wrl1+yo
c
c  compute delivery sigmas
c
      aa=acep(wrw1,dswv(kk,jts))
      ab=aa
      ac=acep(wrl1,dslv(1,kk,jts))
      ad=ac
c
c  compute pod
c
      pod=p(a,aa,b,ab,c,ac,d,ad)
      return
c
c  lock section
c
c  if air-burst set pod to zero
c
 200  if(hob1.gt.0.001)goto 500
c
c   determine weapon radii
c
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
c
      wr=(wrl2-wrl1)/2.0
      if(inl(2,kk,jts).gt.0)wr=(wrl1-wrl2)/2.0
c
c  determine x and y offset distances
c  orien is the target direction
c  azmth is the azimuth from dgz to target
c  xo is the east-west component
c  yo is the north-south component
c
      ddum=di*cnm2ft
      angle=(azmth-orien*10.0)/dpr
      xo=ddum*sin(angle)
      yo=ddum*cos(angle)
c
c  compute boundaries
c
      w=vnw(ign,jts)
      sl=vnl(igv,jts)
c
      a=-w/2.0-wrw1+xo
      b= w/2.0+wrw1+xo
c
      aa=acep(wrw1,dswv(kk,jts))
      ab=aa
c
      if(inl(2,kk,jts).gt.0)goto 210
c
      c=-sl/2.0-wrl1+yo
      d= sl/2.0+wrl2+yo
c
      ac=acep(wrl1,dslv(1,kk,jts))
      ad=acep(wrl2,dslv(2,kk,jts))
      goto 220
c
 210  c=-sl/2.0-wrl2+yo
      d= sl/2.0+wrl1+yo
      ac=acep(wrl2,dslv(2,kk,jts))
      ad=acep(wrl1,dslv(1,kk,jts))
c
 220  poc=p(a,aa,b,ab,c,ac,d,ad)
      return
c
c  dam section
c
c  jts = 4 for dams
c
 300  if(hob1.gt.0.001)goto 500
c
c  determine weapon radii
c
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
c
      wr=(wrw1-wrl1)/2.0
c
c  determine x and y offset distances
c  orien is the target direction
c  azmth is the azimuth from dgz to target
c  xo is the east-west component
c  yo is the north-south component
c
      ddum=di*cnm2ft
      angle=(azmth-orien*10.0)/dpr
      xo=ddum*sin(angle)
      yo=ddum*cos(angle)
c
c  compute boundaries
c
      w=vnw(ign,jts)
      sl=vnl(igv,jts)
c
      c=-sl/2.0+yo
      d= sl/2.0+yo
      if(kf.eq.9)goto 310
c
      a=-wrw1-0.10+xo
      b= wrl1-0.10+xo
      goto 320
c
 310  a=-wrw1+w/2.0+xo
      b= wrl1-w/2.0+xo
c
 320  aa=acep(wrw1,dswv(kk,jts))
      ab=acep(wrl1,dslv(2,kk,jts))
      ac=acep(sl/2.0,dslv(1,kk,jts))
      ad=ac
c
      pod=p(a,aa,b,ab,c,ac,d,ad)
c
      if(pod.lt.zero)goto 500
      return
c
c  special case section
c
 400  call wrcalc(yld,hob1,inl(1,kk,jts),inl(2,kk,jts),inl(3,kk,jts),
     *  dslv(1,kk,jts),wrl1,ierr)
c
      wrw1=wrl1
c
c  determine x and y offset distances
c  orien is the target direction
c  azmth is the azimuth from dgz to target
c  xo is the east-west component
c  yo is the north-south component
c
      ddum=di*cnm2ft
      angle=(azmth-orien*10.0)/dpr
      xo=ddum*sin(angle)
      yo=ddum*cos(angle)
c
c  compute boundaries
c
      w=vnw(ign,jts)
      sl=vnl(igv,jts)
c
      a=-w/2.0-wrw1+xo
      b= w/2.0+wrw1+xo
      c=-sl/2.0-wrl1+yo
      d= sl/2.0+wrl1+yo
c
      aa=acep(wrw1,dswv(kk,jts))
      ab=aa
      ac=acep(wrw1,dslv(1,kk,jts))
      ad=ac
c
      pod=p(a,aa,b,ab,c,ac,d,ad)
      return
c
 500  pod=zero
c
      return
      end
