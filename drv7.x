      subroutine drv7

      include "real8.h"
      character jti*1,kfi*1,dummy*132,inpname*132,pname*132,
     *  fname*132,dname*132
      include "basicd.h"
      include "const.h"
      include "files.h"
      include 'cdkwr.h'

      namelist /plst/ ivn,jti,kfi,yld,
     *  cep,az,fname

      lin  =  1
      lout =  6
      ldbg = 62
      ldbg = -1

      call acon

      ivn  =  1
      jti  = 'p'
      kfi  = '0'

      yld  =     1.0d0

      cep  = 0.0d0
      r95  = 0.0000002d0

      az   = 0.0d0

      iflg = 2

      inpname = 'drv7.nml'
      pname   = '/home/harper/nuclear/OVP/overpressure_output'
      fname   = '1.thin'

      do iarg=1,iargc()
         jarg = iarg + 1

         call getarg(iarg,dummy)

         dummy = dummy(1:lnblnk(dummy))

         if (dummy.eq."-i") then
            call getarg(jarg,inpname)
         endif
      enddo

      open (unit=lin,status='old',file=inpname)
      read(lin,nml=plst)
      close (unit=lin)

      open (unit=lout,status='unknown',file='lout.out')

      if ((ldbg.gt.0).and.(ldbg.ne.lout)) then
         open (unit=ldbg,status='unknown',file='dbg.out')
      endif

      open (unit=10,status='unknown',file='drv7.out')

      dname = pname(1:lnblnk(pname))//"/"//fname(1:lnblnk(fname))

      open (unit=98,status='old',file=dname)

c  inputs are feet
c  need to convert dx to NMi

 111  read(98,*,end=222)dx,hob

      d   = dx / 6076.115d0
      wr  = 0.0d0
      pod = 0.0d0

      call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,
     *            d,wr,pod,iflg,az)

      write(16,10)hob,dx,wr,pod

      goto 111
 222  close (unit=98)

      close (unit=10)
      close (unit=lout)
      if ((ldbg.gt.0).and.(ldbg.ne.lout))close(unit = ldbg)

      return
 10   format(5(1x,f15.5))
      end
