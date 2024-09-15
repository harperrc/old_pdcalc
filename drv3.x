      subroutine drv3

      include "real8.h"
      character jti*1,kfi*1,fname*132,vntk*4
      include "basicd.h"
      include "const.h"
      include "files.h"
      include 'cdkwr.h'

      lin  =  1
      lout =  6
      ldbg = 62
      ldbg = -1

      yld = 300.0d0
      hobi = 0.0d0
      cep  = 3.00d0

      open (unit=lout,status='unknown',file='lout.out')

      if ((ldbg.gt.0).and.(ldbg.ne.lout)) then
         open (unit=ldbg,status='unknown',file='dbg.out')
      endif

      call acon

      fname = "/home/harper/nuclear/OPEN-RISOP/good"
      open (unit=1,status='old',file=fname)

      iflg = 2

 111  read(1,*,end=222)line,catcode,xlat,xlon,hgt,r95,vntk,ivn,jti,kfi
      call pdcalc(ivn,jti,kfi,yld,hobi,r95,cep,d,wr,pod,iflg,az)
      write(6,10)xlon,xlat,r95,ivn,jti,kfi,wr,pod
      goto 111
 222  close (unit=1)

      close (unit=lout)
      if ((ldbg.gt.0).and.(ldbg.ne.lout))close (unit=ldbg)

      stop

 10   format(2(1x,f15.5),1x,f10.3,1x,i2,1x,a,1x,a,2(1x,f15.5))

      end
