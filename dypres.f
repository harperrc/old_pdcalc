      subroutine dpcoeff(m,y,a,b,c,d,e,f,g,h,i,j,k,l)

      include 'real8.h'

      double precision i,j,k,l,m

c  m xq / x
c  y kft/yld**0.33333333

c equations from psr report 1419_3 for peak horizontal dynamic pressure

      l = log10(m)
      a = -236.1d0 + (17.72d0 * m**0.593d0) / 
     *   (1.0d0 + 10.4d0 * m**3.124d0)
      b = 12.27d0 - (21.69d0 * m**2.24d0) / 
     *   (1.0d0 + 6.976d0 * m**0.484d0)
      c = 20.26d0 + (14.7d0 * m**2d0) / 
     *   (1.0d0 + 0.08747d0 * m**3.05d0)
      d = -1.137d0 - (0.5606d0 * m**0.895d0) / 
     *   (1.0d0 + 3.406d0 * m**7.48d0)
      e = 1.731d0 + (10.84d0 * m**1.12d0) / 
     *   (1.0d0 + 12.26d0 * m**0.0014d0)
      f = 2.84d0 + (0.855d0 * m**0.9d0) / 
     *   (1.0d0 + 1.05d0 * m**2.84d0)
      g = 50.0d0 - (1843.0d0 * y**2.153d0) / 
     *   (1.0d0 + 3.95d0 * y**5.08d0)
      h = 0.294d0 + (71.56d0 * y**8.7d0) / 
     *   (1.0d0 + 115.3d0 * y**6.186d0)
      i = abs(-3.324d0 + (987.5d0 * y**4.77d0) / 
     *   (1.0d0 + 211.8d0 * y**5.166d0))
      j = 1.955d0 + (169.7d0 * y**9.317d0) / 
     *   (1.0d0 + 97.36d0 * y**6.513d0)
      k = 8.123d-6 + (0.001613d0 * y**6.428d0) / 
     *   (1.0d0 + 60.26d0 * y**7.358d0)

      return
      end
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
