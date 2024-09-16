      subroutine wrcalc(yld,hob1,iv,jjt,kf,dsig,wr,ierr)

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
      include 'cdkwr.h'

c  replaced equivalence statements with arrays setup as data statements
c  used pgf77 to compile code and wrote the results of wp & wq out then
c  reformatted with python to yield the data statements below (rch 09/09/24)

      dimension wp(8,2,10),wq(8,10),tvnp(9),tvnq(9)

      data (wp( 1, 1,ii),ii=1,10) /
     *  8.2069360d+00, 8.2912300d+00, 8.3952230d+00, 8.4195800d+00,
     *  8.4994890d+00, 8.5259850d+00, 8.5862220d+00, 8.6559620d+00,
     *  8.6812850d+00, 8.7196540d+00/

      data (wp( 1, 2,ii),ii=1,10) /
     *  8.2634300d+00, 8.2995900d+00, 8.3952230d+00, 8.4195800d+00,
     *  8.4994890d+00, 8.5259850d+00, 8.5862220d+00, 8.6559620d+00,
     *  1.2513420d+01, 1.3472890d+01/

      data (wp( 2, 1,ii),ii=1,10) /
     * -9.8662220d-02,-1.1329390d-01,-1.4717856d-01,-9.9827820d-02,
     * -1.0965210d-01,-6.3120550d-02,-1.0027110d-01,-1.3679890d-01,
     * -1.1432860d-01,-1.2158530d-01/

      data (wp( 2, 2,ii),ii=1,10) /
     * -1.2109524d-01,-1.1043340d-01,-1.4717856d-01,-9.9827820d-02,
     * -1.0965210d-01,-6.3120550d-02,-1.0027110d-01,-1.3679890d-01,
     * -1.5163440d+00,-1.9719830d+00/

      data (wp( 3, 1,ii),ii=1,10) /
     * -4.2705320d-03, 3.1199080d-04, 1.2744890d-02,-4.1872797d-03,
     * -3.4445750d-03,-2.5622190d-02,-9.9171760d-03, 1.4262810d-02,
     * -1.7888690d-03, 1.2036040d-03/

      data (wp( 3, 2,ii),ii=1,10) /
     *  1.2746600d-03,-4.8494085d-04, 1.2744890d-02,-4.1872797d-03,
     * -3.4445750d-03,-2.5622190d-02,-9.9171760d-03, 1.4262810d-02,
     *  1.7699440d-01, 2.5472670d-01/

      data (wp( 4, 1,ii),ii=1,10) /
     *  4.4673610d-04, 0.0000000d+00,-2.0632770d-03, 5.4490840d-04,
     *  7.2617060d-04, 5.4264470d-03, 2.6023200d-03,-4.0929990d-03,
     *  1.5959090d-04,-1.3863280d-04/

      data (wp( 4, 2,ii),ii=1,10) /
     * -9.2065490d-06, 6.5830100d-05,-2.0632770d-03, 5.4490840d-04,
     *  7.2617060d-04, 5.4264470d-03, 2.6023200d-03,-4.0929990d-03,
     * -8.9008350d-03,-1.4325115d-02/

      data (wp( 5, 1,ii),ii=1,10) /
     *  0.0000000d+00, 0.0000000d+00, 1.6675910d-04,-3.7583520d-05,
     * -7.1090500d-05,-5.9263390d-04,-3.6028220d-04, 5.0281250d-04,
     *  0.0000000d+00, 0.0000000d+00/

      data (wp( 5, 2,ii),ii=1,10) /
     *  0.0000000d+00,-9.1680378d-07, 1.6675910d-04,-3.7583520d-05,
     * -7.1090500d-05,-5.9263390d-04,-3.6028220d-04, 5.0281250d-04,
     *  1.4007360d-04, 2.6403710d-04/

      data (wp( 6, 1,ii),ii=1,10) /
     *  0.0000000d+00, 0.0000000d+00,-6.8934200d-06, 1.4009690d-06,
     *  3.3190130d-06, 3.4855040d-05, 2.8025150d-05,-2.5712240d-05,
     *  0.0000000d+00, 0.0000000d+00/

      data (wp( 6, 2,ii),ii=1,10) /
     *  0.0000000d+00, 0.0000000d+00,-6.8934200d-06, 1.4009690d-06,
     *  3.3190130d-06, 3.4855040d-05, 2.8025150d-05,-2.5712240d-05,
     *  0.0000000d+00, 0.0000000d+00/

      data (wp( 7, 1,ii),ii=1,10) /
     *  0.0000000d+00, 0.0000000d+00, 1.4237140d-07,-2.1079890d-08,
     * -5.6685050d-08,-1.0228650d-06,-1.0826360d-06, 4.3790030d-07,
     *  0.0000000d+00, 0.0000000d+00/

      data (wp( 7, 2,ii),ii=1,10) /
     *  0.0000000d+00, 0.0000000d+00, 1.4237140d-07,-2.1079890d-08,
     * -5.6685050d-08,-1.0228650d-06,-1.0826360d-06, 4.3790030d-07,
     *  0.0000000d+00, 0.0000000d+00/

      data (wp( 8, 1,ii),ii=1,10) /
     *  0.0000000d+00, 0.0000000d+00,-1.1675015d-09, 0.0000000d+00,
     *  0.0000000d+00, 1.1443200d-08, 1.5415570d-08, 0.0000000d+00,
     *  0.0000000d+00, 0.0000000d+00/

      data (wp( 8, 2,ii),ii=1,10) /
     *  0.0000000d+00, 0.0000000d+00,-1.1675015d-09, 0.0000000d+00,
     *  0.0000000d+00, 1.1443200d-08, 1.5415570d-08, 0.0000000d+00,
     *  0.0000000d+00, 0.0000000d+00/

      data (wq( 1,jj),jj=1,10) /
     *  8.3151590d+00, 8.3760820d+00, 8.4202400d+00, 8.4853150d+00,
     *  8.5760000d+00, 8.6435000d+00, 8.6866970d+00, 8.7074500d+00,
     *  8.7363280d+00, 8.7930420d+00/

      data (wq( 2,jj),jj=1,10) /
     * -1.0608700d-01,-1.0429500d-01,-1.0947300d-01,-1.0313900d-01,
     * -1.0398900d-01,-1.1105640d-01,-1.1648200d-01,-1.1755000d-01,
     * -1.1518050d-01,-1.1548850d-01/

      data (wq( 3,jj),jj=1,10) /
     *  5.2240000d-04,-1.2014000d-03, 1.4622880d-03,-3.4114000d-03,
     * -6.5788000d-03,-4.1904000d-03, 3.6340000d-04, 2.3483000d-03,
     *  2.1175000d-03, 1.8710000d-04/

      data (wq( 4,jj),jj=1,10) /
     * -3.1300000d-04,-3.9113600d-05,-5.9697200d-04, 3.0870000d-04,
     *  1.2382000d-03, 6.6440000d-04,-6.1690000d-04,-1.3054000d-03,
     * -1.5218000d-03,-1.1008000d-03/

      data (wq( 5,jj),jj=1,10) /
     *  3.2265000d-05, 1.2875700d-05, 6.6970020d-05,-1.0726700d-05,
     * -1.3330000d-04,-7.7684800d-05, 8.5754100d-05, 1.9090000d-04,
     *  2.6540000d-04, 2.3570000d-04/

      data (wq( 6,jj),jj=1,10) /
     * -1.2322700d-06,-4.9757900d-07,-3.0149460d-06, 3.1566200d-07,
     *  8.0138700d-06, 5.9869500d-06,-4.0726300d-06,-1.1520000d-05,
     * -1.9675000d-05,-2.0156200d-05/

      data (wq( 7,jj),jj=1,10) /
     *  1.9670700d-08, 5.7725700d-09, 6.1882280d-08,-5.5664600d-09,
     * -2.3468400d-07,-2.2707900d-07, 5.6650200d-08, 2.8307900d-07,
     *  6.1801500d-07, 6.9752000d-07/

      data (wq( 8,jj),jj=1,10) /
     * -1.0588000d-10, 0.0000000d+00,-4.8666330d-10, 0.0000000d+00,
     *  2.5129500d-09, 3.0062600d-09, 0.0000000d+00,-2.4470400d-09,
     * -7.2056200d-09,-8.7486600d-09/

c  yield limits for p targets

      data tvnp/54.0d0 , 51.0d0 , 34.0d0 , 30.0d0 , 27.0d0 , 
     *          27.0d0 , 22.0d0 , 21.0d0 , 20.0d0/

c  yield limits for q targets

      data tvnq/35.0d0 , 35.0d0 , 35.0d0 , 31.0d0 , 28.0d0 , 
     *          26.0d0 , 25.0d0 , 23.0d0 , 22.0d0/

      ierr=0

      if(kf.ge.10) then
         ierr=6
         return
      endif

      jt = jjt
      vn = iv
      fk = kf

      yldcu = yld**third
      yldic = one / yldcu

      shob = hob1 * yldic
      ds2  = one / (one - dsig * dsig)

      fk10 = fk * 0.10d0

      sil = shob / 100.0d0 + 1.0001d0
      il  = int(sil)
      il  = max(1,il)

      if(il.ge.10) then
         ierr = 3
         return
      endif

      fac = (il - 1) * 100.0d0
      fac = (shob - fac) / 100.0d0

c  check for p or q

      if(jt.eq.1)goto 240

c  calculate the adjusted vn for q type targets

      r2 = 3.0d0

 10   r1    = one - fk10 * (one - 2.7144176d0 * yldic * (r2**third))
      abdif = r1 - r2
      r2    = r1
      abdif = abs(abdif)
      if(abdif.gt.0.001d0)goto 10

      avn = vn + 2.742d0 * log(r2)
      ax  = 1.10d0

c  compute wr for q type targets

      if(avn.gt.tvnq(il)) then
         ierr = 2
         return
      endif

      swrl=wq(1,il)+avn*(wq(2,il)+avn*(wq(3,il)+avn*(wq(4,il)+
     *  avn*(wq(5,il)+avn*(wq(6,il)+avn*(wq(7,il)+avn*wq(8,il)))))))
      ih=il+1
      swrh=wq(1,ih)+avn*(wq(2,ih)+avn*(wq(3,ih)+avn*(wq(4,ih)+
     *  avn*(wq(5,ih)+avn*(wq(6,ih)+avn*(wq(7,ih)+avn*wq(8,ih)))))))
      goto 300

c  calculate the adjusted vn for p type targets

 240  r2 = 2.0

 11   r1    = one - fk10 * (one - 2.7144176d0 * yldic * (r2**0.50d0))
      abdif = r1 - r2
      r2    = r1
      abdif = abs(abdif)
      if(abdif.gt.0.001)goto 11

      avn = vn + 5.485d0 * log(r2)
      ax  = 1.04d0

c  compute wr for p type targets

      if(avn.gt.tvnp(il)) then
         ierr = 2
         return
      endif

      if(avn.lt.36.0d0)goto 260

c  functional fit to high vn range

      shck = -9.0d0 * avn + 560.0d0

      if(shob.gt.shck) then
         ierr = 2
         return
      endif

c  calculate wr using the AIM fit

      vx = (avn - 46.0d0) / 10.0d0
      wo = 88.0d0 - vx * (53.0d0 - vx * (21.0d0 - vx * 8.0d0))

      if(shob.ne.zero)goto 259

      wr = wo * yldcu * ds2 / ax
      goto 400

 259  hm   = 70.0d0 - 5.0d0 * vx * (7.0d0 - vx)
      con  = 1.60d0 + 0.20d0 * vx
      hx   = shob / hm
      dw   = hm - wo
      wr   = wo + dw * hx * (2.0d0 - hx - con * (one - hx)**2)
      wr   = wr * yldcu * ds2 / ax
      goto 400

c  use the table data

 260  j=1
      if(avn.gt.7.5d0)j=2

      swrl=wp(1,j,il)+avn*(wp(2,j,il)+avn*(wp(3,j,il)+avn*(wp(4,j,il)+
     *  avn*(wp(5,j,il)+avn*(wp(6,j,il)+avn*(wp(7,j,il)+avn*wp(8,j,il))
     *  )))))

      ih=il+1
      swrh=wp(1,j,ih)+avn*(wp(2,j,ih)+avn*(wp(3,j,ih)+avn*(wp(4,j,ih)+
     *  avn*(wp(5,j,ih)+avn*(wp(6,j,ih)+avn*(wp(7,j,ih)+avn*wp(8,j,ih))
     *  )))))

 300  swrl = exp(swrl)
      swrh = exp(swrh)
      wr   = (swrl + fac * (swrh - swrl)) * yldcu * ds2 / ax

 400  if(wr.lt.zero)wr=zero

      return
      end
