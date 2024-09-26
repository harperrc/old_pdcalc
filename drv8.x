      subroutine drv8

      include "real8.h"
      character jti*1,kfi*1
      include "basicd.h"
      include "const.h"
      include "files.h"
      include 'cdkwr.h'
      include 'cdkpd.h'

      lin  =  1
      lout =  6
      ldbg = 62
      ldbg = -1

      call acon

      if ((ldbg.gt.0).and.(ldbg.ne.lout)) then
         open (unit=ldbg,status='unknown',file='dbg.out')
      endif

      ivn  =  27
      jti  = 'p'
      kfi  = '0'

      yld  =     1.0d0

      cep  = 0.0d0
      r95  = 0.0000002d0

      az   = 0.0d0

      iflg = 2

      hbmax  = 1000.0d0 * yld**0.33333333333d0

      hbmax  = 8000.0d0
      offmax = 8000.00d0

      hbmax  = 300.0d0
      offmax = 300.0d0

      dhob   = 1.0d0
      doff   = 1.0d0

      hob = 0.0d0
      pdmax = -1.0d0
      hmax  = 0.0d0
      gmax  = 0.0d0

      do while (hob.le.hbmax)
         off  = 0.0d0
         do while (off.le.offmax)
            wr   = 0.0d0
            offnm = off / cnm2ft

            call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,
     *                  offnm,wr,pod,iflg,az)

            ovp = overp(off,hob,yld)
            ovp = max(1.0d-10,min(ovp,1.0e5))

            write(10,10)hob,off,wr,pod,ierr,ovp

            if (pod.ge.0.99d0.and.off.ge.gmax) then
               hmax  = hob
               gmax  = off
               omax  = ovp
            endif

            off = off + doff
         enddo
         write(10,*)''
         hob = hob + dhob
      enddo

      write(6,20)'gmax',gmax
      write(6,20)'hmax',hmax
      write(6,30)'omax',omax
      write(6,40)'ncall',ncall

      if ((ldbg.gt.0).and.(ldbg.ne.lout))close(unit = ldbg)

      return

 10   format(4(1x,f15.5),1x,i3,1pe15.7)
 20   format(a10,1x,f15.5)
 30   format(a10,1x,1pe15.7)
 40   format(a10,1x,i10)

      end
