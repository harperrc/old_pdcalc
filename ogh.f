      subroutine ogh(ivn,jti,kfi,yld,r95,cep,az,podd,ohob,ogrn)

c  given a vntk, yield, r95 and cep determine the 'optimum' 
c  height of burst. that is height of burst which give then
c  maximum weapon radius acheiving a desired POD.

c  uses ridders method (chapter 9.2 of numerical recipies) in offset

      include 'real8.h'
      character jti*1,kfi*1
      include 'cdkpd.h'
      include 'cdkwr.h'

      iflg = 2

      y13 = yld**0.3333333333d0

      hbmax = 1000.0d0 * y13
      hbmax = 2.0 * hbmax
      ofmax = 2.0d0 * hbmax

      ohob = -1.0d0
      ogrn = -1.0e0

c  step size in height of burst

      dhob = 1.0d0

c  set size in initial search for bounds in offset

      bigs = 50.0d0

      xacc   = 1.0d-10
      maxi   = 100
      rtflsp = 0.0d0

      hob  = 0.0d0

      do while (hob.le.hbmax)

c  take big steps to find starting points

         x1   = 0.0d0
         x2   = ofmax

         offx = 0.0d0

c  first x1

         do while (offx.le.ofmax)
            offnm = offx / 6076.115d0

            wr   = 0.0d0
            pod1 = 0.0d0

            call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,offnm,
     *               wr,pod1,iflg,az)

            if (pod1.ge.podd.and.ierr.eq.0) then
               x1 = offx
               goto 101
            endif

            offx = offx + bigs
         enddo

         goto 104

 101     offx = x1

c  first x2

         do while (offx.le.ofmax)
            offnm = offx / 6076.115d0
            wr   = 0.0d0
            pod2 = 0.0d0

            call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,offnm,
     *               wr,pod2,iflg,az)

            if (pod2.lt.podd.and.ierr.eq.0) then
               x2 = offx
               goto 102
            endif

            offx = offx + bigs
         enddo

c  now iterate to solution (ridders method)

 102     offnm = x1 / 6076.115d0
         wr  = 0.0d0
         pod = 0.0d0
         call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,offnm,
     *               wr,pod,iflg,az)
         fl = pod - podd

         offnm = x2 / 6076.115d0
         wr  = 0.0d0
         pod = 0.0d0
         call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,offnm,
     *               wr,pod,iflg,az)
         fh = pod - podd

c  left out check for fl being negative as the above always makes
c  fl positive

         xl = x2
         xh = x1
         sw = fl
         fl = fh
         fh = sw

         dx = xh - xl

         do j=1,maxi
            if (abs(fl-fh).le.0.0d0)goto 103

            rtflsp = xl + dx * fl / (fl - fh)

            off    = rtflsp / 6076.115d0

            wr  = 0.0d0
            pod = 0.0d0

            call pdcalc(ivn,jti,kfi,yld,hob,r95,cep,off,
     *                  wr,pod,iflg,az)

            f = pod - podd

            if (ierr.ne.0)f = sign(1.0d0,f)

            if (f.lt.0.0d0) then
               del = xl - rtflsp
               xl  = rtflsp
               fl  = f
            else
               del = xh - rtflsp
               xh  = rtflsp
               fh  = f
            endif

            dx = xh - xl

            if (abs(del).lt.xacc.or.abs(f).le.1.0e-10)goto 103
         enddo

c  keep up with largest ground range and associated height of burst

 103     if (rtflsp.ge.ogrn) then
            ogrn = rtflsp
            ohob = hob
         endif

 104     hob = hob + dhob
      enddo

      return
      end
