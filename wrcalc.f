      subroutine wrcalc(yld,hob1,iv,jjt,kf,dsig,wr,ierr)
c
c  compute weapon radius for p (overpressure) or q (dynamic pressure)
c  targets
c
c  inputs
c        yld   weapon yield (kt)
c        hob1  height of burst (feet)
c        iv    vulnerability number
c        jjt   target type 
c              1-p
c              2-q
c        kf    yield adjustment factor (0-9)
c        dsig  damage sigma
c
c  outputs:
c        wr    weapon radius (feet)
c        ierr  error status flag
      include "real8.h"
      include "const.h"
      include "files.h"

c  increased dim of wq1 from 72 to 80 and dropped wq2 array
c  wq2 incorporated in wq1 array as modern compilers dont have limit on # of continuations 

      dimension wp(8,2,10),wq(8,10),tvnp(9),tvnq(9)

      data (wp( 1, 1,ii),ii=1,10) /
     *  8.2069360e+00, 8.2912300e+00, 8.3952230e+00, 8.4195800e+00,
     *  8.4994890e+00, 8.5259850e+00, 8.5862220e+00, 8.6559620e+00,
     *  8.6812850e+00, 8.7196540e+00/

      data (wp( 1, 2,ii),ii=1,10) /
     *  8.2634300e+00, 8.2995900e+00, 8.3952230e+00, 8.4195800e+00,
     *  8.4994890e+00, 8.5259850e+00, 8.5862220e+00, 8.6559620e+00,
     *  1.2513420e+01, 1.3472890e+01/

      data (wp( 2, 1,ii),ii=1,10) /
     * -9.8662220e-02,-1.1329390e-01,-1.4717856e-01,-9.9827820e-02,
     * -1.0965210e-01,-6.3120550e-02,-1.0027110e-01,-1.3679890e-01,
     * -1.1432860e-01,-1.2158530e-01/

      data (wp( 2, 2,ii),ii=1,10) /
     * -1.2109524e-01,-1.1043340e-01,-1.4717856e-01,-9.9827820e-02,
     * -1.0965210e-01,-6.3120550e-02,-1.0027110e-01,-1.3679890e-01,
     * -1.5163440e+00,-1.9719830e+00/

      data (wp( 3, 1,ii),ii=1,10) /
     * -4.2705320e-03, 3.1199080e-04, 1.2744890e-02,-4.1872797e-03,
     * -3.4445750e-03,-2.5622190e-02,-9.9171760e-03, 1.4262810e-02,
     * -1.7888690e-03, 1.2036040e-03/

      data (wp( 3, 2,ii),ii=1,10) /
     *  1.2746600e-03,-4.8494085e-04, 1.2744890e-02,-4.1872797e-03,
     * -3.4445750e-03,-2.5622190e-02,-9.9171760e-03, 1.4262810e-02,
     *  1.7699440e-01, 2.5472670e-01/

      data (wp( 4, 1,ii),ii=1,10) /
     *  4.4673610e-04, 0.0000000e+00,-2.0632770e-03, 5.4490840e-04,
     *  7.2617060e-04, 5.4264470e-03, 2.6023200e-03,-4.0929990e-03,
     *  1.5959090e-04,-1.3863280e-04/

      data (wp( 4, 2,ii),ii=1,10) /
     * -9.2065490e-06, 6.5830100e-05,-2.0632770e-03, 5.4490840e-04,
     *  7.2617060e-04, 5.4264470e-03, 2.6023200e-03,-4.0929990e-03,
     * -8.9008350e-03,-1.4325115e-02/

      data (wp( 5, 1,ii),ii=1,10) /
     *  0.0000000e+00, 0.0000000e+00, 1.6675910e-04,-3.7583520e-05,
     * -7.1090500e-05,-5.9263390e-04,-3.6028220e-04, 5.0281250e-04,
     *  0.0000000e+00, 0.0000000e+00/

      data (wp( 5, 2,ii),ii=1,10) /
     *  0.0000000e+00,-9.1680378e-07, 1.6675910e-04,-3.7583520e-05,
     * -7.1090500e-05,-5.9263390e-04,-3.6028220e-04, 5.0281250e-04,
     *  1.4007360e-04, 2.6403710e-04/

      data (wp( 6, 1,ii),ii=1,10) /
     *  0.0000000e+00, 0.0000000e+00,-6.8934200e-06, 1.4009690e-06,
     *  3.3190130e-06, 3.4855040e-05, 2.8025150e-05,-2.5712240e-05,
     *  0.0000000e+00, 0.0000000e+00/

      data (wp( 6, 2,ii),ii=1,10) /
     *  0.0000000e+00, 0.0000000e+00,-6.8934200e-06, 1.4009690e-06,
     *  3.3190130e-06, 3.4855040e-05, 2.8025150e-05,-2.5712240e-05,
     *  0.0000000e+00, 0.0000000e+00/

      data (wp( 7, 1,ii),ii=1,10) /
     *  0.0000000e+00, 0.0000000e+00, 1.4237140e-07,-2.1079890e-08,
     * -5.6685050e-08,-1.0228650e-06,-1.0826360e-06, 4.3790030e-07,
     *  0.0000000e+00, 0.0000000e+00/

      data (wp( 7, 2,ii),ii=1,10) /
     *  0.0000000e+00, 0.0000000e+00, 1.4237140e-07,-2.1079890e-08,
     * -5.6685050e-08,-1.0228650e-06,-1.0826360e-06, 4.3790030e-07,
     *  0.0000000e+00, 0.0000000e+00/

      data (wp( 8, 1,ii),ii=1,10) /
     *  0.0000000e+00, 0.0000000e+00,-1.1675015e-09, 0.0000000e+00,
     *  0.0000000e+00, 1.1443200e-08, 1.5415570e-08, 0.0000000e+00,
     *  0.0000000e+00, 0.0000000e+00/

      data (wp( 8, 2,ii),ii=1,10) /
     *  0.0000000e+00, 0.0000000e+00,-1.1675015e-09, 0.0000000e+00,
     *  0.0000000e+00, 1.1443200e-08, 1.5415570e-08, 0.0000000e+00,
     *  0.0000000e+00, 0.0000000e+00/

      data (wq( 1,jj),jj=1,10) /
     *  8.3151590e+00, 8.3760820e+00, 8.4202400e+00, 8.4853150e+00,
     *  8.5760000e+00, 8.6435000e+00, 8.6866970e+00, 8.7074500e+00,
     *  8.7363280e+00, 8.7930420e+00/

      data (wq( 2,jj),jj=1,10) /
     * -1.0608700e-01,-1.0429500e-01,-1.0947300e-01,-1.0313900e-01,
     * -1.0398900e-01,-1.1105640e-01,-1.1648200e-01,-1.1755000e-01,
     * -1.1518050e-01,-1.1548850e-01/

      data (wq( 3,jj),jj=1,10) /
     *  5.2240000e-04,-1.2014000e-03, 1.4622880e-03,-3.4114000e-03,
     * -6.5788000e-03,-4.1904000e-03, 3.6340000e-04, 2.3483000e-03,
     *  2.1175000e-03, 1.8710000e-04/

      data (wq( 4,jj),jj=1,10) /
     * -3.1300000e-04,-3.9113600e-05,-5.9697200e-04, 3.0870000e-04,
     *  1.2382000e-03, 6.6440000e-04,-6.1690000e-04,-1.3054000e-03,
     * -1.5218000e-03,-1.1008000e-03/

      data (wq( 5,jj),jj=1,10) /
     *  3.2265000e-05, 1.2875700e-05, 6.6970020e-05,-1.0726700e-05,
     * -1.3330000e-04,-7.7684800e-05, 8.5754100e-05, 1.9090000e-04,
     *  2.6540000e-04, 2.3570000e-04/

      data (wq( 6,jj),jj=1,10) /
     * -1.2322700e-06,-4.9757900e-07,-3.0149460e-06, 3.1566200e-07,
     *  8.0138700e-06, 5.9869500e-06,-4.0726300e-06,-1.1520000e-05,
     * -1.9675000e-05,-2.0156200e-05/

      data (wq( 7,jj),jj=1,10) /
     *  1.9670700e-08, 5.7725700e-09, 6.1882280e-08,-5.5664600e-09,
     * -2.3468400e-07,-2.2707900e-07, 5.6650200e-08, 2.8307900e-07,
     *  6.1801500e-07, 6.9752000e-07/

      data (wq( 8,jj),jj=1,10) /
     * -1.0588000e-10, 0.0000000e+00,-4.8666330e-10, 0.0000000e+00,
     *  2.5129500e-09, 3.0062600e-09, 0.0000000e+00,-2.4470400e-09,
     * -7.2056200e-09,-8.7486600e-09/


c
c  yield limits for p targets
c
      data tvnp/54.,51.,34.,30.,27.,27.,22.,21.,20./
c
c  yield limits for q targets
c
      data tvnq/35.,35.,35.,31.,28.,26.,25.,23.,22./
c
      ierr=0
c
      if(kf.ge.10) then
         ierr=6
         return
      endif
c
      jt=jjt
      vn=iv
      fk=kf
c
      yldcu=yld**third
      yldic=one/yldcu
c
      shob=hob1*yldic
      ds2=one/(one-dsig*dsig)
c
      fk10=fk*0.10
c
      sil=shob/100.0+1.0001
      il=int(sil)
      il=max(1,il)
c
      if(il.ge.10) then
         ierr=3
         return
      endif
c
      fac=(il-1)*100.0
      fac=(shob-fac)/100.0
c
c  check for p or q
c
      if(jt.eq.1)goto 240
c
c  calculate the adjusted vn for q type targets
c
      r2=3.0
c
 10   r1=one-fk10*(one-2.7144176*yldic*(r2**third))
      abdif=r1-r2
      r2=r1
      abdif=abs(abdif)
      if(abdif.gt.0.001)goto 10
c
      avn=vn+2.742*log(r2)
      ax=1.10
c
c  compute wr for q type targets
c
      if(avn.gt.tvnq(il)) then
         ierr=2
         return
      endif
c
      swrl=wq(1,il)+avn*(wq(2,il)+avn*(wq(3,il)+avn*(wq(4,il)+
     *  avn*(wq(5,il)+avn*(wq(6,il)+avn*(wq(7,il)+avn*wq(8,il)))))))
      ih=il+1
      swrh=wq(1,ih)+avn*(wq(2,ih)+avn*(wq(3,ih)+avn*(wq(4,ih)+
     *  avn*(wq(5,ih)+avn*(wq(6,ih)+avn*(wq(7,ih)+avn*wq(8,ih)))))))
      goto 300
c
c  calculate the adjusted vn for p type targets
c
 240  r2=2.0
c
 11   r1=one-fk10*(one-2.7144176*yldic*(r2**0.50))
      abdif=r1-r2
      r2=r1
      abdif=abs(abdif)
      if(abdif.gt.0.001)goto 11
c
      avn=vn+5.485*log(r2)
      ax=1.04
c
c  compute wr for p type targets
c
      if(avn.gt.tvnp(il)) then
         ierr=2
         return
      endif
c
      if(avn.lt.36.0)goto 260
c
c  functional fit to high vn range
c
      shck=-9.0*avn+560.0
      if(shob.gt.shck) then
         ierr=2
         return
      endif
c
c  calculate wr using the AIM fit
c
      vx=(avn-46.0)/10.0
      wo=88.0-vx*(53.0-vx*(21.0-vx*8.0))
      if(shob.ne.zero)goto 259
c
      wr=wo*yldcu*ds2/ax
      goto 400
c
 259  hm=70.0-5.0*vx*(7.0-vx)
      con=1.6+0.20*vx
      hx=shob/hm
      dw=hm-wo
      wr=wo+dw*hx*(2.0-hx-con*(one-hx)**2)
      wr=wr*yldcu*ds2/ax
      goto 400
c
c  use the table data
c
 260  j=1
      if(avn.gt.7.5)j=2
      swrl=wp(1,j,il)+avn*(wp(2,j,il)+avn*(wp(3,j,il)+avn*(wp(4,j,il)+
     *  avn*(wp(5,j,il)+avn*(wp(6,j,il)+avn*(wp(7,j,il)+avn*wp(8,j,il))
     *  )))))
      ih=il+1
      swrh=wp(1,j,ih)+avn*(wp(2,j,ih)+avn*(wp(3,j,ih)+avn*(wp(4,j,ih)+
     *  avn*(wp(5,j,ih)+avn*(wp(6,j,ih)+avn*(wp(7,j,ih)+avn*wp(8,j,ih))
     *  )))))
c
 300  swrl=exp(swrl)
      swrh=exp(swrh)
      wr=(swrl+fac*(swrh-swrl))*yldcu*ds2/ax
c
 400  if(wr.lt.zero)wr=zero
c
      return
      end
