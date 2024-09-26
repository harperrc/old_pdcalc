      subroutine drv6

      include "real8.h"
      character jti*1,kfi*1,dummy*132,fname*132
      include "basicd.h"
      include "const.h"
      include "files.h"
      include 'cdkwr.h'

      namelist /plst/ ivn,jti,kfi,yld,d0,d1,nd,r95,
     *  cep,az,hob0,hob1,dhob

      lin  =  1
      lout =  6
      ldbg = 62
      ldbg = -1

      call acon

      ivn  =  1
      jti  = 'p'
      kfi  = '0'

      yld  =     1.0d0
      hob0 =     0.0d0
      hob1 =  8000.0d0
      dhob =    20.0d0

      d0   =     0.0d0
      d1   =     10000.0d0 / 6076.115d0
      nd   = 64
      dd   = (d1 - d0) / (nd - 1.0d0)

      cep  = 0.0d0
      r95  = 0.2d0

      az   = 0.0d0

      iflg = 2

      fname = 'drv6.nml'

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

      open (unit=lout,status='unknown',file='lout.out')

      if ((ldbg.gt.0).and.(ldbg.ne.lout)) then
         open (unit=ldbg,status='unknown',file='dbg.out')
      endif

      open (unit=10,status='unknown',file='drv6.out')

      hob = hob0
      do while (hob.le.hob1)
         nout = 0
         dx  = d0
         do while (dx.le.d1)
            d   = dx
            wr  = 0.0d0
            pod = 0.0d0

            call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,
     *                  d,wr,pod,iflg,az)

             if (pod.gt.0.0d0) then
                nout = nout + 1
                write(16,10)hob,dx * 6076.115d0,wr,pod
             endif

             dx = dx + dd
         enddo

         if (nout.gt.0) then
            write(16,*)''
         endif

         hob = hob + dhob
      enddo

      close (unit=10)
      close (unit=lout)
      if ((ldbg.gt.0).and.(ldbg.ne.lout))close(unit = ldbg)

      return
 10   format(5(1x,f15.5))
      end
