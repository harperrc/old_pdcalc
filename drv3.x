      subroutine drv3

      include "real8.h"
      character jti*1,kfi*1,fname*132,vntk*4,nname*132,dummy*132
      include "basicd.h"
      include "const.h"
      include "files.h"
      include 'cdkwr.h'

c  yld   yield (kt)
c  hobi  height of burst for evaluation (ft)
c  cep   circular error probable for weapon (km)
c  fname name of reformatted RISOP file

c        fields (can be read free format since no character data)
c           line,catcode,xlat,xlon,hgt,r95,vntk,ivn,jti,kfi

c                  line   line # in file (usefull to reference back to original file)
c                  xlat   latitude (deg)
c                  xlon   longtude (deg)
c                  hgt    height (ft)
c                  r95    95 % damage radius (km)
c                  vntk   full vntk (e.g. 10P0)
c                  ivn    vn number portion only
c                  jti    type (e.g. p,q,r....) N.B. MUST BE LOWER CASE
c                  kfi    k-factor


      namelist /plst/ yld,hobi,cep,fname

      call acon

      lin  =  1
      lout =  6
      ldbg = 62
      ldbg = -1

      yld = 300.0d0
      hobi = 0.0d0
      cep  = 3.00d0

      nname = 'drv3.nml'
      fname = 'good'

      do iarg=1,iargc()
         jarg = iarg + 1

         call getarg(iarg,dummy)

         dummy = dummy(1:lnblnk(dummy))

         if (dummy.eq."-i") then
            call getarg(jarg,nname)
         endif
      enddo

      open (unit=lin,status='old',file=nname)
      read(lin,nml=plst)
      close (unit=lin)

      open (unit=lout,status='unknown',file='lout.out')

      if ((ldbg.gt.0).and.(ldbg.ne.lout)) then
         open (unit=ldbg,status='unknown',file='dbg.out')
      endif

      open (unit=1,status='old',file=fname)

      iflg = 2

 111  read(1,*,end=222)line,catcode,xlat,xlon,hgt,r95,vntk,ivn,jti,kfi

      d   = 0.0d0
      wr  = 0.0d0
      pod = 0.0d0

      call pdcalc(ivn,jti,kfi,yld,hobi,r95,cep,d,wr,pod,iflg,az)

      write(6,10)xlon,xlat,r95,ivn,jti,kfi,wr,pod

      goto 111
 222  close (unit=1)

      close (unit=lout)
      if ((ldbg.gt.0).and.(ldbg.ne.lout))close (unit=ldbg)

      stop

 10   format(2(1x,f15.5),1x,f10.3,1x,i2,1x,a,1x,a,2(1x,f15.5))

      end
