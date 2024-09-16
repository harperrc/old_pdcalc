      subroutine drv4

      include "real8.h"
      parameter (mxmsl=64)
      character jti*1,kfi*1,fname*132,header*132,
     *  tnato*22,tussr*20,akey*20,jtinp*1,iname*132,
     *  dummy*132,tname*132,
     *  nato(mxmsl)*22,ussr(mxmsl)*20
    
      include "basicd.h"
      include "const.h"
      include "files.h"
      include 'cdkwr.h'

      dimension ylda(mxmsl),cepa(mxmsl)

c  iname  filename containing icbm information
c         this file requires a strict format as follows
c                a22,1x,a20,1x,i5,1x,i5,1x,f12.5,1x,f12.5,1x,f12.5,1x,i3
c         fields
c                 tnato,tussr,nsoft,nhard,yld,rmax,cep,numwh

c                 tnato  NATO name for missile
c                 tussr  USSR name for missile
c                 nsoft  # of missiles in/on soft launchers (future use)
c                 hnard  # of missiles in/on hard launchers (future use)
c                 yld    yield (Kt)
c                 rmax   maximum range (km) (future use)
c                 cep    cep of missile (ft)
c                 numwh  # of warheads of this type/missile (future use)
c  tname  filename containing the target types, r95, vntk broken down
c         this file requires a strict format as follows
c               i5,1x,a20,1x,f17.5,1x,i2,1x,a,1x,a
c         fields
c                num,akey,r95,ivn,jtinp,kfi

c                num   line # from original file
c                akey  VNTK-R95 (used as key in python reformatting code)
c                ivn   VN portion
c                jtinp target type (note here this is in caps and code will convert to lowercase)
c                kfi   k-factor

c  hob0   starting hob (ft)
c  hob1   ending hob (ft)
c  dhob   hob step size (ft)


      namelist /plst/ iname,tname,hob0,hob1,dhob

      call acon

      lin  =  1
      lout =  6
      ldbg = 62
      ldbg = -1

      iname = "ussr.dat"
      tname = "types.tgt"

      hob0  =     0.0d0
      hob1  = 10000.0d0
      dhob  =   200.0d0

      fname = 'drv4.nml'

      do iarg=1,iargc()
         jarg = iarg + 1

         call getarg(iarg,dummy)

         dummy = dummy(1:lnblnk(dummy))

         if (dummy.eq."-i") then
            call getarg(jarg,fname)
         endif
      enddo

      open (unit=lin,status='old',file=fname)
      read(lin,nml=plst)
      close (unit=lin)

      if ((ldbg.gt.0).and.(ldbg.ne.lout)) then
         open (unit=ldbg,status='unknown',file='dbg.out')
      endif

      open (unit=16,status='unknown',file='drv4.out')

c  read in missile types, yields, cep,....

      open (unit=1,status='old',file=iname)

      read(1,10)header

      nmsl = 0

 333  read(1,20,end=444)tnato,tussr,nsoft,nhard,
     *                  yld,rmax,cep,numwh

      if (rmax.lt.7000.0d0)goto 333

      nmsl = nmsl + 1

      nato(nmsl) = tnato
      ussr(nmsl) = tussr
      ylda(nmsl) = yld
      cepa(nmsl) = cep * cnm2ft

      goto 333

 444  close (unit=1)

      open (unit=1,status='old',file=tname)

      iflg = 2
      d    = 0.0d0

c  read in a summary of the target types available in the RISOP
c  file
 
 111  read(1,30,end=222)num,akey,r95,ivn,jtinp,kfi

c  convert to lower case

      ic = iachar(jtinp)
      if (ic.ge.60.and.ic.le.90) then
         jti = char(ic + 32)
      else
         jti = jtinp
      endif

      do imsl=1,nmsl
         yld = ylda(imsl)
         cep = cepa(imsl)
         hob = hob0

         do while (hob.le.hob1)
            d   = 0.0d0
            wr  = 0.0d0
            pod = 0.0d0

            call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,d,wr,pod,iflg,az)

            write(16,40)avn,hob,yld,cep,r95,wr,pod,ivn,jti,kfi,imsl

            hob = hob + dhob
         enddo
         write(16,*)''
      enddo
      write(16,*)''

      goto 111
 222  close (unit=1)

      if ((ldbg.gt.0).and.(ldbg.ne.lout))close (unit=ldbg)

      close (unit=16)

      stop

 10   format(a)
 20   format(a22,1x,a20,1x,i5,1x,i5,1x,f12.5,1x,f12.5,1x,f12.5,1x,i3)
 30   format(i5,1x,a20,1x,f17.5,1x,i2,1x,a,1x,a)
 40   format(7(1x,f15.5),1x,i2,1x,a,1x,a,1x,i3)

      end
