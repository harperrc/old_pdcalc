      double precision function dypres(grft_,hobft_,yld_)

c  grft_    ground range from burst (ft)
c  hobft_   height of burst (ft)
c  yld-     yield (kt)

c equations from psr report 1419_3 for peak horizontal dynamic pressure

      include 'real8.h'
      double precision i,i1,j,j1,k,k1,l,l1,m

      yld    = yld_
      yld13  = yld**0.33333333333333d0

c  convert to kft

      grkft  = grft_ / 1000.0d0
      hobkft = hobft_ / 1000.0d0

      x      = max(1.0d-8,grkft / yld13)
      y      = max(1.0d-8,hobkft / yld13)
      r      = sqrt(x * x + y * y)

      xq     = (63.5d0 * y**7.26d0) / 
     *            (1.0d0 + 67.11d0 * y**4.746d0) + 
     *             0.6953d0 * y**0.808d0

      m = max(1.0d-6,xq / x)

      call dpcoeff(m,y,a,b,c,d,e,f,g,h,i,j,k,l)
 
      j = min(150.0d0,max(-150.0d0,j))
      e = min(150.0d0,max(-150.0d0,e))

      qs = (a * r**d) / (1.0 + b * r**e) + c / r**f

      if (x.ge.xq) then
         dypres = qs
         return
      endif

c  if in mach region then evaluate above term first at m = 1

      call dpcoeff(1.0d0,y,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1)

      rp = sqrt(xq * xq + y * y)
      qm = (a1 * rp**d1) / (1.0d0 + b1 * rp**e1) + c1 / rp**f1

c  apply other terms

      t1 = (g * l**i) / (1.0d0 + 649.0d0 * l**i)
      t2 = (4.01d0 * l**j) / (1.0d0 + h * l**j)
      t3 = 7.67d-6 * (1.0d0 / (k + l**3.22d0) - 1.0d0 / k)
      arg = t1 - t2 + t3

      qs = qm * exp(arg)

      dypres = qs

      return
      end
