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

      dimension wp(8,2,10),wq(8,10),tvnp(9),tvnq(9),wp1(88),wp2(72),
     *  wq1(80)
c
ccc      equivalence (wp(1),wp1(1)),(wp(89),wp2(1)),(wq(1),wq1(1))
      equivalence (wp(1,1,1),wp1(1)),(wp(89),wp2(1)),(wq(1,1),wq1(1))
c
c  coefficients for 7th order polynomial approximation for weapon
c  radius coputations (reformatted c.harper)
c
      data wp1 /
     *  8.2069360d+00, -9.8662220d-02, -4.2705320d-03,  4.4673610d-04, 
     *  0.0000000d+00,  0.0000000d+00,  0.0000000d+00,  0.0000000d+00, 
     *  8.2634300d+00, -1.2109524d-01,  1.2746600d-03, -9.2065490d-06, 
     *  0.0000000d+00,  0.0000000d+00,  0.0000000d+00,  0.0000000d+00, 
     *  8.2912300d+00, -1.1329390d-01,  3.1199080d-04,  0.0000000d+00, 
     *  0.0000000d+00,  0.0000000d+00,  0.0000000d+00,  0.0000000d+00, 
     *  8.2995900d+00, -1.1043340d-01, -4.8494085d-04,  6.5830100d-05, 
     * -9.1680378d-07,  0.0000000d+00,  0.0000000d+00,  0.0000000d+00, 
     *  8.3952230d+00, -1.4717856d-01,  1.2744890d-02, -2.0632770d-03, 
     *  1.6675910d-04, -6.8934200d-06,  1.4237140d-07, -1.1675015d-09, 
     *  8.3952230d+00, -1.4717856d-01,  1.2744890d-02, -2.0632770d-03, 
     *  1.6675910d-04, -6.8934200d-06,  1.4237140d-07, -1.1675015d-09, 
     *  8.4195800d+00, -9.9827820d-02, -4.1872797d-03,  5.4490840d-04, 
     * -3.7583520d-05,  1.4009690d-06, -2.1079890d-08,  0.0000000d+00, 
     *  8.4195800d+00, -9.9827820d-02, -4.1872797d-03,  5.4490840d-04, 
     * -3.7583520d-05,  1.4009690d-06, -2.1079890d-08,  0.0000000d+00, 
     *  8.4994890d+00, -1.0965210d-01, -3.4445750d-03,  7.2617060d-04, 
     * -7.1090500d-05,  3.3190130d-06, -5.6685050d-08,  0.0000000d+00, 
     *  8.4994890d+00, -1.0965210d-01, -3.4445750d-03,  7.2617060d-04, 
     * -7.1090500d-05,  3.3190130d-06, -5.6685050d-08,  0.0000000d+00, 
     *  8.5259850d+00, -6.3120550d-02, -2.5622190d-02,  5.4264470d-03, 
     * -5.9263390d-04,  3.4855040d-05, -1.0228650d-06,  1.1443200d-08 /

      data wp2 /
     *  8.5259850d+00, -6.3120550d-02, -2.5622190d-02,  5.4264470d-03, 
     * -5.9263390d-04,  3.4855040d-05, -1.0228650d-06,  1.1443200d-08, 
     *  8.5862220d+00, -1.0027110d-01, -9.9171760d-03,  2.6023200d-03, 
     * -3.6028220d-04,  2.8025150d-05, -1.0826360d-06,  1.5415570d-08, 
     *  8.5862220d+00, -1.0027110d-01, -9.9171760d-03,  2.6023200d-03, 
     * -3.6028220d-04,  2.8025150d-05, -1.0826360d-06,  1.5415570d-08, 
     *  8.6559620d+00, -1.3679890d-01,  1.4262810d-02, -4.0929990d-03, 
     *  5.0281250d-04, -2.5712240d-05,  4.3790030d-07,  0.0000000d+00, 
     *  8.6559620d+00, -1.3679890d-01,  1.4262810d-02, -4.0929990d-03, 
     *  5.0281250d-04, -2.5712240d-05,  4.3790030d-07,  0.0000000d+00, 
     *  8.6812850d+00, -1.1432860d-01, -1.7888690d-03,  1.5959090d-04, 
     *  0.0000000d+00,  0.0000000d+00,  0.0000000d+00,  0.0000000d+00, 
     *  1.2513420d+01, -1.5163440d+00,  1.7699440d-01, -8.9008350d-03, 
     *  1.4007360d-04,  0.0000000d+00,  0.0000000d+00,  0.0000000d+00, 
     *  8.7196540d+00, -1.2158530d-01,  1.2036040d-03, -1.3863280d-04, 
     *  0.0000000d+00,  0.0000000d+00,  0.0000000d+00,  0.0000000d+00, 
     *  1.3472890d+01, -1.9719830d+00,  2.5472670d-01, -1.4325115d-02, 
     *  2.6403710d-04,  0.0000000d+00,  0.0000000d+00,  0.0000000d+00 /

      data wq1 /
     *  8.3151590d+00, -1.0608700d-01,  5.2240000d-04, -3.1300000d-04, 
     *  3.2265000d-05, -1.2322700d-06,  1.9670700d-08, -1.0588000d-10, 
     *  8.3760820d+00, -1.0429500d-01, -1.2014000d-03, -3.9113600d-05, 
     *  1.2875700d-05, -4.9757900d-07,  5.7725700d-09,  0.0000000d+00, 
     *  8.4202400d+00, -1.0947300d-01,  1.4622880d-03, -5.9697200d-04, 
     *  6.6970020d-05, -3.0149460d-06,  6.1882280d-08, -4.8666330d-10, 
     *  8.4853150d+00, -1.0313900d-01, -3.4114000d-03,  3.0870000d-04, 
     * -1.0726700d-05,  3.1566200d-07, -5.5664600d-09,  0.0000000d+00, 
     *  8.5760000d+00, -1.0398900d-01, -6.5788000d-03,  1.2382000d-03, 
     * -1.3330000d-04,  8.0138700d-06, -2.3468400d-07,  2.5129500d-09, 
     *  8.6435000d+00, -1.1105640d-01, -4.1904000d-03,  6.6440000d-04, 
     * -7.7684800d-05,  5.9869500d-06, -2.2707900d-07,  3.0062600d-09, 
     *  8.6866970d+00, -1.1648200d-01,  3.6340000d-04, -6.1690000d-04, 
     *  8.5754100d-05, -4.0726300d-06,  5.6650200d-08,  0.0000000d+00, 
     *  8.7074500d+00, -1.1755000d-01,  2.3483000d-03, -1.3054000d-03, 
     *  1.9090000d-04, -1.1520000d-05,  2.8307900d-07, -2.4470400d-09, 
     *  8.7363280d+00, -1.1518050d-01,  2.1175000d-03, -1.5218000d-03, 
     *  2.6540000d-04, -1.9675000d-05,  6.1801500d-07, -7.2056200d-09, 
     *  8.7930420d+00, -1.1548850d-01,  1.8710000d-04, -1.1008000d-03, 
     *  2.3570000d-04, -2.0156200d-05,  6.9752000d-07, -8.7486600d-09 /

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

      do i=1,8
         do j=1,2
            do k=1,10
               write(43,43)i,j,k,wp(i,j,k)
            enddo
         enddo
      enddo
 43   format(3(1x,i4),1x,1pe15.7)

      do i=1,8
         do j=1,10
            write(44,44)i,j,wq(i,j)
         enddo
      enddo
 44   format(2(1x,i4),1x,1pe15.7)
      stop
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
      il=sil
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
