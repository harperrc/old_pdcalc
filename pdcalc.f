      subroutine pdcalc(iv,jti,kfi,yld,hob1,r95,cep,d,wr,pod,iiflg,
     *   azmth)
c
c  compute damage levels or associated values
c
c  inputs
c    iv      vulnerability number or target dimensions for equivalent
c            target area (eta) type targets
c    jti     target type (character*1)
c            valid (r,s,q,t,u,l,p,m,n,o,z,y,a,b,c,d,e,f,x)
c    kfi     k-factor  (character*1)
c            (0-9 for p and q type)
c            (a-p for x type)
c   yld      weapon yield (kt)
c   hob1     height of burst of weapon (feet)
c   r95      radius of a circle encompassing 95 percent of the 
c            circular normal target area (nmi)
c            for eta targets r95*10 = orientation of the target (degrees)
c   cep      circular error probable of the specified weapon system (feet)
c
c  inputs/outputs
c   d        distance from dgz to target (nmi)
c   wr       weapon radius (feet)
c   pod      probability of achieving the specified level of damage
c   iiflg    control flag
c
c    value     function              vntk             (d)       (pod)     (wr)
c    -----     --------              ----            -----      ------    ------
c      1   compute pod and wr        all             input      output    output
c          (max pod = 0.99)
c
c      2   compute pod and wr        all             input      output    output
c          (max pod = 0.999)
c
c     3,4  compute wr                not eta         input        na      output
c
c     5,6  compute d and wr          not eta         output     input     output
c
c      7   compute fatalities and    "x" only        input      output    output
c          casualties                                           (fatl)    (casu)
c
c      8   compute pod and wr        not eta         input      in-dsig   output
c                                                               out-pod
c
c      9   compute pod               all             input      in-pod    in-wr
c                                                               out-pod   out-wr
c
c   azmth   azimuth in degrees from dgz to target
c              
      include "real8.h"
      character*1 jt,kft,kfi,jti,kfn,jtd
      include "const.h"
      include "files.h"
c
      dimension ddsig(19),jjtd(19),kfn(27),kff(27),jtd(19)
c
      data jtd  /'r','s','q','t','u','l','p','m','n','o',
     *           'z','y','a','b','c','d','e','f','x'/
      data jjtd/  2 , 2 , 2 , 2 , 2 , 1 , 1 , 1 , 1 , 1,
     *            4 , 4 , 5 , 6 , 7 , 8 , 9 ,10 , 3/
      data ddsig/.1 , .2, .3, .4, .5, .1, .2, .3, .4, .5,
     *           .3 , .3, 1., 1., 1., 1., 1., 1., 1./
c
      data kfn/'0','1','2','3','4','5','6','7','8','9','a','b',
     *  'c','d','e','f','g','h','i','j','k','l','m','n','o','p','q'/
      data kff/ 0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 1 , 2 ,
     *   3 , 4 , 5 , 6 , 7 , 8 , 9 , 10, 11, 12, 13, 14, 15, 16, 17/
c
c  convert to lower case
c
      ich=ichar(jti)
      if(ich.gt.64.and.ich.le.90) then
         jt=char(ichar(jti)+32)
      else
         jt=jti
      endif
c
      ich=ichar(kfi)
      if(ich.gt.64.and.ich.le.90) then
         kft=char(ichar(kfi)+32)
      else
         kft=kfi
      endif
c
c  get target type
c
      do i=1,19
         if(jt.eq.jtd(i)) then
            jjt=jjtd(i)
            dsig=ddsig(i)
            goto 10
         endif
      enddo
c
      ierr=9
      goto 990
c
c  get kf (numeric)
c
 10   do i=1,27
         if(kft.eq.kfn(i)) then
            kf=kff(i)
            goto 11
         endif
      enddo
c
      ierr=9
      goto 990
c
 11   ifhflg=0
      ifgflg=0
c
      iflg=iiflg
c
      if(iflg.gt.1000)ifhflg=1
      if(iflg.gt.1000)iflg=iflg-1000
      if(iflg.gt.100)ifgflg=1
      if(iflg.gt.100)iflg=iflg-100
      if(iflg.eq.3)iflg=4
      if(iflg.eq.5)iflg=6
c
      iflh=iflg
c
c  iflg of 7 must have a 'x' vntk
c
      if(iflg.ne.7)goto 14
c
c  'x' vn number
c
      if(jjt.eq.3)goto 100
c
      ierr=5
      goto 990
c
 14   if(jjt.gt.2)goto 100
c
c  overpressure, dynamic pressure and crater type vn's
c
 15   if(iflg.eq.8.or.iflg.eq.9)dsig=pod
      if(iflg.ge.9)goto 20
c
      call wrcalc(yld,hob1,iv,jjt,kf,dsig,wr,ierr)
      if(ierr.ne.0)goto 990
c
 19   if(iflg.ne.4)goto 20
      pod=zero
      return
c
 20   call lncalc(cep,dsig,wr,r95,pod,d,iflh,ierr)
      if(ierr.ne.0)goto 990
      return
c
 100  if(jjt.le.4)goto 200
c
c  eta type vntk
c
      if(iflg.le.2)goto 110
c
      ierr=4
      goto 990
c
 110  jts=jjt-4
c
      call etcalc(iv,jts,kf,yld,cep,hob1,r95,azmth,d,pod,wr,ierr)
      if(ierr.ne.0)goto 990
      return
c
c  check for and process 'x', 'y', and 'z' type vntk
c
 200  if(jt.ne.'z')goto 210
      jjt=1
c
      if(hob1.le.0.99)goto 15
c
      ierr=10
      goto 990
c
 210  if(jt.eq.'x')goto 225
      if(hob1.le.0.99)goto 220
c
 220  call wrclcy(kf,yld,wr,ierr)
      if(ierr.eq.0)goto 19
c
c  invalid jt='y' vntk; set pod,wr and or d to zero and return
c
      if(iflg.eq.6)d=zero
      if(iflg.ne.9.and.iflg.ne.10)wr=zero
      if(iflg.ne.6)pod=zero
      return
c
 225  iflh=2
      if(iflg.ne.7)goto 230
      kk=kf/2*2
      if(kf.ne.kk)goto 230
      ierr=11
      goto 990
c
 230  call wrpers(yld,hob1,iv,jjt,kf,dsig,wr,ierr)
      if(ierr.ne.0)goto 990
      if(iflg.eq.4)return
c
      call lncalc(cep,dsig,wr,r95,pod,d,iflh,ierr)
      if(ierr.ne.0)goto 990
      if(iflg.ne.7)return
c
      if(kf/2*2.eq.kf)goto 231
      p1=pod
      kf=kf+1
      goto 230
c
 231  wr=pod
      pod=p1
      return
c
 990  if(ifhflg.eq.1.and.ierr.eq.10)goto 991
      if(ifgflg.eq.1.and.ierr.eq.2)goto 991
c
      call errmsg(ierr,iv,jjt,kf,yld,cep,hob1,r95,d,wr,pod,iflg)
      return
c
 991  if(iflg.eq.5.or.iflg.eq.6)d=zero
      if(iflg.ne.9.and.iflg.ne.10)wr=zero
      if(iflg.ne.3.and.iflg.ne.6)pod=zero
c
      return
      end
