      subroutine drv9

      include "real8.h"
      character jti*1,kfi*1,iname*132,dummy*132
      include "basicd.h"
      include "const.h"
      include "files.h"
      include 'cdkwr.h'
      include 'cdkpd.h'
      include 'sab.h'

      namelist /lst9/ ivn,jti,kfi,yld,cep,r95,az,iflg,
     *   hob0,dhob,hobmx,gr0,dgr,grmx,pkmax

      lin  =  1
      lout =  6
      ldbg = 62
      ldbg = 77

      call acon

      if ((ldbg.gt.0).and.(ldbg.ne.lout)) then
         open (unit=ldbg,status='unknown',file='dbg.out')
      endif

      ivn  =  13
      jti  = 'q'
      kfi  = '7'

      ivn  =  21
      jti  = 'q'
      kfi  = '9'

      yld  =   100.0d0

      cep  = 1.0d0
      r95  = 2.0d-6

      az   = 0.0d0

      iflg = 2
      hob0  =     0.0d0
      dhob  =   100.0d0
      hobmx = 10000.0d0
      gr0   =     0.0d0
      dgr   =   100.0d0
      grmx  = 10000.0d0
      pkmax =     0.98d0

      iname = 'drv9.nml'
      if (iargc().gt.1)call getarg(2,iname)

      open (unit=1,status='old',file=iname)
 111  read(1,nml=lst9,end=222)

      write(dummy,50)int(yld),ivn,jti,kfi,'2'
      dummy = adjustl(dummy)
      open(unit=2,status='unknown',file=dummy)

      write(dummy,50)int(yld),ivn,jti,kfi,'4'
      dummy = adjustl(dummy)
      open(unit=4,status='unknown',file=dummy)

      gr0   = gr0 / cnm2ft
      dgr   = dgr / cnm2ft
      grmx  = grmx / cnm2ft
      hobmx = 900.0d0 * yld**0.33333333333333333333d0

      grndmax = 0.0d0
      hobmax  = 0.0d0
      hob = hob0

      do while (hob.le.hobmx)
         hobkm = hob / 3280.8d0

         offnm = 0.0d0
         wr    = 0.0d0
         pod   = 0.0d0

         gr = gr0

         do while (gr.le.grmx)
            call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,
     *                  gr,wr,pod,iflg,az)

            grkm = gr * 1.852d0
            wrkm = wr / 3280.8d0

c  impose limits to keep dypres & overp from blowing up

            grft = max(1.0d-8,gr * cnm2ft)
            hobft = max(1.0d-8,hob)

       pres = 0.0
            if (jti.eq.'q') then
               pres = dypres(grft,hobft,yld)
               pres_em = qs_em
            else if (jti.eq.'p') then
               pres = overp(grft,hobft,yld)
               pres_em = pres
            else
               pres = 0.0
               pres_em = pres
            endif

            pres = min(1.0d5,pres)

            write(2,10)hobkm,grkm,wrkm,pod,pres,pres_em

            if (pod.ge.pkmax) then
               if (grkm.ge.grndmax) then
                  grndmax = grkm
                  hobmax  = hobkm
                  wrmax   = wrkm
                  pd      = pod
               endif
            endif
            
            gr = gr + dgr
         enddo
         write(2,*)''
         hob = hob + dhob
      enddo

      write(6,30)yld,ivn,jti,kfi,avn
      write(6,20)grndmax,hobmax,'km'
      write(6,20)grndmax * 3280.8,hobmax * 3280.8,'ft'
      write(4,10)grndmax,hobmax,wrmax,pd

      grft = grndmax * 3280.8d0
      hbft = hobmax  * 3280.8d0

      ovp = overp(grft,hbft,yld)
      dyp = dypres(grft,hbft,yld)
      write(6,40)'ovp = ',ovp
      write(6,40)'dyp = ',dyp,qs_em
      write(6,*)''

      gr0  = gr0 * cnm2ft
      dgr  = dgr * cnm2ft
      grmx = grmx * cnm2ft

      close (unit=2)
      close (unit=4)
  
      goto 111
 222  close (unit=1)

      stop
 10   format(4(1x,f15.5),2(1x,1pe12.3))
 20   format('gr =',1x,f10.3,1x,
     *       'hob =',1x,f10.3,1x,
     *       a)
 30   format('yld =',1x,f10.3,1x,
     *       'vn  =',1x,i2,1x,
     *       'T   =',1x,a,1x,
     *       'K   =',1x,a,1x,
     *       'Adj VN =',1x,f10.3)
 40   format(a,2(1x,f10.3))
 50   format(i4,'-'i2,a,a,'.',a)
      end
