      subroutine drv1

c  mode = 1
c     reads a file of hob, ground range and yield and calls, looping over VN (ivns,ivne,ivns)
c     fixed k-factor and tgttype

c  mode = 2
c     read a files of ground range, hob, yld, vn, tgttype, k-factor 

      include "real8.h"
      character fname*132,gname*132,jti*1,kfi*1,whichm*10,sq*1
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
c  iflg  1 product pod up to value of 0.990, d must be input
c        2 product pod up to value of 0.999, d must be input
c        3 see 4
c        4 produce weapon radius
c        5 see 6
c        6 produce d, maximum distance at which a give pod
c          can be acheived, pod must be input
c        7 produce fatality pod and casualty pod
c          returned in pod & wr, d must be input
c        8 damage sigma is input through pod variable pod is
c          output, d is input
c        9 damage sigma and weapon radius are input, pod is
c          output, d is input
c       10  wr input, pod is output, d is input
c  mode  which read to use
c  gamma reentry angle assumed 
c  az    azimuth iin degrees from dgz to target
c
      namelist /simlst/ cep,r95,gname,ivns,ivne,ivnd,jti,kfi,iflg,
     *  mode,gamma,az
c
      sq   = char(39)

      lin  =  1
      lout =  6
      ldbg = 62
      ldbg = -1
c
      open (unit=lout,status='unknown',file='lout.out')
c
      if((ldbg.gt.0).and.(ldbg.ne.lout)) then
         open (unit=ldbg,status='unknown',file='dbg.out')
      endif
c
      if(iargc().gt.0) then
         call getarg(1,fname)
         open (unit=lin,status='old',file=fname)
      else
         open (unit=lin,status='old',file='pdcalc.inp')
      endif

      call acon

      ivns  = 1
      ivne  = 23
      ivnd  = 1

      jti   = 'z'
      kfi   = '0'
      iflg  = 2

      cep   = zero
      r95   = zero
      iflg  = 0

      mode   = 1

      gamma  = -1.0d0

      az     = 0.0d0

      read(lin,simlst)
      write(lout,simlst)

      close (unit=lin)

c  convert to feet

      cep = cep * ckm2ft
      r95 = r95 * ckm2ft

c  convert r95 to nautical miles

      r95   = r95 / cnm2ft

      gamma = gamma / dpr

c  open file with info

      open (unit=1,status='old',file=gname)

      open (unit=20,status='unknown',file='gnu.des')
      write(20,10)'set xyplane 0'
      write(20,10)'set grid z vertical'

      open (unit=13,status='unknown',file='sendmethis.txt')

      smalpk = 1.0d-8

      if(mode.eq.2) then
         whichm = "mode2"
         open (unit=2,status='unknown',file='mode2.out')
         write(2,*)'grn hob yld iv jti kfi wr pod'
         goto 333
      else
         whichm = "mode1"
         open (unit=2,status='unknown',file='mode1.out')
         write(2,11)
      endif

      iread = 0

 111  read(1,*,end=222)grn,hob,yld

c  for y and z target types force ground bursts

      iread = iread + 1

      if(jti.eq.'z'.or.jti.eq.'y')hob=zero

      grf = grn * ckm2ft
      hof = hob * ckm2ft

      d = grf / cnm2ft

      do 100 iv=ivns,ivne,ivnd

      if (iread.eq.1) then
         write(20,20)iv
         write(20,21)sq,whichm(1:lnblnk(whichm)),sq,iv,4,sq,sq
         write(20,22)iv,5
         write(20,23)
      endif

      write(13,*)grn,hob,yld
      call flush(13)

c  compute pd with and without cep effects

      do j=1,2
         if(j.eq.1) then
            xcep = zero
            xr95 = zero
         else
            xcep = cep
            xr95 = r95
         endif

         call pdcalc(iv,jti,kfi,yld,hof,xr95,xcep,d,wr,pod,iflg,az)

         pd(j) = max(smalpk,pod)
      enddo

c  compute scaled range

      scl=sqrt(grn * grn + hob * hob) / yld**third

c N.B. pd is array of 2

      write(2,30)iv,scl,wr/ckm2ft,pd,grn,hob

 100  continue
      write(2,*)''
c
      goto 111
 222  close (unit=1)
c
      close (unit=lout)
      close (unit=2)
      close (unit=13)
      close (unit=20)
c
      stop
c
 333  read(1,40,end=444)grn,hob,yld,iv,jti,kfi

c  force hob = 0 for crater targets...add in approximation
c  if tgt flown to ground

      if(jti.eq.'y'.or.jti.eq.'z') then
         if(gamma.gt.zero)grn = grn + hob / tan(gamma)
         hob=zero
      endif

      grf = grn * ckm2ft
      hof = hob * ckm2ft

      d = grf / cnm2ft

      xr95 = zero
      xcep = zero

      call pdcalc(iv,jti,kfi,yld,hof,xr95,xcep,d,wr,pod,iflg,az)

      write(2,50)grn,hob,yld,iv,jti,kfi,wr/ckm2ft,pod

      goto 333
 444  close (unit=1)

      close (unit=lout)
      close (unit=2)
      if ((ldbg.gt.0).and.(ldbg.ne.lout))close (unit=ldbg)

      stop

 10   format(a)
 11   format(4x,'iv',10x,'scl',14x,'wr',11x,'pd_wcep',
     *       8x,'pc_nocep',10x,'grn',12x,'hob')
 20   format("set title 'VN =",1x,i2)
 21   format("spl",1x,a,a,".out",a,1x,
     *  "u ($1 == ",i2," ? $2 : 1/0):7:",i2,1x,
     *  "@ml1 tit",1x,a,"cep=0",a,1x,",\\")
 22   format(13x,"'' u ($1 == ",i2," ? $2 : 1/0):7:",
     *  i2,1x,"@ml1 tit 'cep>0'")
 23   format("pause -1")
 30   format(i5,1x,6(1x,f15.8))
 40   format(3f10.3,i5,a,a)
 50   format(3(1x,f10.3),1x,i5,1x,a,1x,a,2(1x,f12.5))

      end
