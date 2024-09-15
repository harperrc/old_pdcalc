      subroutine drv2

      include "real8.h"
      character jti*1,kfi*1
      include "basicd.h"
      include "const.h"
      include "files.h"
      include 'cdkwr.h'

      lin  =  1
      lout =  6
      ldbg = 62
      ldbg = -1

      open (unit=lout,status='unknown',file='lout.out')

      if ((ldbg.gt.0).and.(ldbg.ne.lout)) then
         open (unit=ldbg,status='unknown',file='dbg.out')
      endif

      call acon

      ivnb =  1 
      ivne = 30
      ivns =  1

      jti  = 'p'

      kfb  = 0
      kfs  = 1

      yld  = 1000.0d0
      hobi = 0.0d0

      r95  = 5.0d0
      cep  = 0.0d0

      az   = 0.0d0

      iflg = 2

      open (unit=10,status='unknown',file='drv2.out')

      do ivn=ivnb,ivne,ivns
         do kf=kfb,min(ivn-1,9),kfs
            kfi = char(kf + 48)

            d   = 0.0d0
            wr  = 0.0d0

            call pdcalc(ivn,jti,kfi,yld,hobi,r95,cep,
     *                  d,wr,pod,iflg,az)

            write(10,20)ivn,kf,avn,d,wr,pod
         enddo
         write(10,*)
      enddo
      close (unit=10)

      iflg = 6

      open (unit=11,status='unknown',file='iflg6.out')

      do ivn=ivnb,ivne,ivns
         do kf=kfb,min(ivn-1,9),kfs
             kfi = char(kf + 48)

            if (kf.eq.kfb)icnt = 0
            pod = 0.01
            do while (pod.le.1.0d0)

               call pdcalc(ivn,jti,kfi,yld,hobi,r95,cep,
     *                     d,wr,pod,iflg,az)

               write(11,30)ivn,kf,avn,d,wr,pod,icnt
               icnt = icnt + 1

               pod = pod + 0.01
            enddo
            write(11,*)''
         enddo
         write(11,*)''
      enddo
      close (unit=11)

      close (unit=lout)
      if ((ldbg.gt.0).and.(ldbg.ne.lout))close(unit = ldbg)

      return

 20   format(2(1x,i3),4(1x,f15.5))
 30   format(2(1x,i3),4(1x,f15.5),1x,i5)

      end
