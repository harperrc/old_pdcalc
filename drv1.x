      subroutine drv1

      include "real8.h"
      character*132 fname,gname
      character*1 jti,kfi
c
      include "basicd.h"
      include "const.h"
      include "files.h"
c
      dimension pd(2)
c
c  cep   circular error probable for weapon (km)
c  r95   95% of damage radius lies within this area (km)
c  gname file name containing ground range, altitude and yield data
c  ivns  starting vn #
c  ivne  ending vn #
c  ivnd  step in vn #
c  jti   target type
c  kfi   k-factor
c  iflg  see pdcalc
c  mode  which read to use
c  gamma reentry angle assumed 
c
      namelist /simlst/ cep,r95,gname,ivns,ivne,ivnd,jti,kfi,iflg,
     *  mode,gamma
c
      lin=1
      lout=6
      ldbg=62
c
      open (unit=lout,status='unknown',file='pdcalc.out')
c
      if(ldbg.ne.lout) then
         open (unit=ldbg,status='unknown',file='pdcalc.dbg')
      endif
c
      if(iargc().gt.0) then
         call getarg(1,fname)
         open (unit=lin,status='old',file=fname)
      else
         open (unit=lin,status='old',file='pdcalc.inp')
      endif
c
      call acon
c
      ivns=1
      ivne=23
      ivnd=1
c
      jti='z'
      kfi='0'
      ifl=2
c
      cep=zero
      r95=zero
c
      mode=1
c
      gamma=-1.0
c
      read(lin,simlst)
      write(lout,simlst)
c
      close (unit=lin)
c
c  convert to feet
c
      cep=cep*ckm2ft
      r95=r95*ckm2ft
c
c  convert r95 to nautical miles
c
      r95=r95/cnm2ft
c
      gamma=gamma/dpr
c
c  open file with info
c
      open (unit=1,status='old',file=gname)
c
      smalpk=1.0e-8
c
      open (unit=2,status='unknown',file='pdcalc.dat')
c
      if(mode.eq.2) then
         goto 333
      endif
c
 111  read(1,*,end=222)grn,hob,yld
c
c  for y and z target types force ground bursts
c
      if(jti.eq.'z'.or.jti.eq.'y')hob=zero
c
      grf=grn*ckm2ft
      hof=hob*ckm2ft
c
      d=grf/cnm2ft
c
      do 100 iv=ivns,ivne,ivnd
c
c  compute pd with and without cep effects
c
      do j=1,2
         if(j.eq.1) then
            xcep=zero
            xr95=zero
         else
            xcep=cep
            xr95=r95
         endif
c
         call pdcalc(iv,jti,kfi,yld,hof,xr95,xcep,d,wr,pod,ifl,az)
c
         pd(j)=pod
      enddo
c
c  limit to smalpk for iplot use
c
      do j=1,2
         pd(j)=max(smalpk,pd(j))
      enddo
c
c  compute scaled range
c
      scl=sqrt(grn*grn+hob*hob)/yld**third
c
      write(2,30)iv,scl,wr/ckm2ft,pd,grn,hob
c
 100  continue
      write(2,*)''
c
      goto 111
 222  close (unit=1)
c
      close (unit=lout)
      close (unit=2)
c
      stop
c
 333  read(1,40,end=444)grn,hob,yld,iv,jti,kfi
c
c  force hob = 0 for crater targets...add in approximation
c  if tgt flown to ground
c
      if(jti.eq.'y'.or.jti.eq.'z') then
         if(gamma.gt.zero)grn=grn+hob/tan(gamma)
         hob=zero
      endif
c
      grf=grn*ckm2ft
      hof=hob*ckm2ft
c
      d=grf/cnm2ft
c
      xr95=zero
      xcep=zero
c
      call pdcalc(iv,jti,kfi,yld,hof,xr95,xcep,d,wr,pod,ifl,az)
c
      write(2,50)grn,hob,yld,iv,jti,kfi,wr/ckm2ft,pod
c
      goto 333
 444  close (unit=1)
c
      close (unit=lout)
      close (unit=2)
c
      stop
c
 30   format(i5,1x,6(1x,f15.8))
 40   format(3f10.3,i5,a,a)
 50   format(3(1x,f10.3),1x,i5,1x,a,1x,a,2(1x,f12.5))
c
      end
