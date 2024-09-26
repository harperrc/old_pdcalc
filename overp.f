      double precision function overp(gr_,hob_,yld_)

c  inputs feet & kt

      include 'real8.h'

      yieldOfBurst  = yld_
      groundRange   = gr_
      heightOfBurst = hob_
   
      cubeRootOfYield = yieldOfBurst**0.33333333333333d0

      x = max(1.0d-8,groundRange / cubeRootOfYield)
      y = max(1.0d-8,heightOfBurst / cubeRootOfYield)
   
      capr  = sqrt(x * x + y * y)
      capr2 = capr * capr
   
      R = capr / 1000.0d0
   
      z = y / x
      z = min(z,88.0d0)
   
      oz = x / y
       
      y2  = y * y
      y4  = y2 * y2
      y5  = y4 * y
       
      z2  = z * z
      z3  = z2 * z
      z5  = z2 * z3
      z6  = z5 * z
      z8  = z5 * z3
      z17 = z8 * z8 * z
      z18 = z17 * z
       
      a  = 1.22d0 - 3.908d0 * z2 / (1.0d0 + 810.2d0 * z5)
   
      b  = 2.321d0 + 6.195d0 * z18 / (1.0d0 + 1.113d0 * z18) - 
     *     0.03831d0 * z17 / (1.0d0 + 0.02415d0 * z17) + 
     *     0.6692d0 / (1.0d0 + 4164.0d0 * z8)
      bb = 0.0629d0 * oz**8.34d0 / 
     *     (1.0d0 + 0.00509d0 * oz**13.05d0) * 0.05d0 * 
     *     y / (1.0d0 + 2.56d-8 * y5)
   
      c  = 4.153d0 - 1.149d0 * z18 / (1.0d0 + 1.641d0 * z18) - 
     *     1.10d0 / (1.0d0 + 2.771d0 * z**2.50d0)
   
      d  = -4.166d0 + 25.76d0 * z**1.75d0 / (1.0d0 + 1.382d0 * z18) + 
     *     8.257d0 * z / (1.0d0 + 3.219d0 * z)
   
      e  = 1.00d0 - 0.004642d0 * z18 / (1.0d0 + 0.003886d0 * z18)
   
      f  = 0.6069d0 + 2.879d0 * z**9.250d0 / 
     *     (1.0d0 + 2.359d0 * z**14.5d0) - 
     *     17.15d0 * z2 / (1.0d0 + 71.66d0 * z3)
   
      g  = 1.83d0 + 5.361d0 * z2 / (1.0d0 + 0.3139d0 * z6)
   
      h = -(0.2905d0 + 64.67d0 * z5) / (1.0d0 + 441.5d0 * z5) - 
     *     1.389d0 * z / (1.0d0 + 49.03d0 * z5) + 
     *     (8.808d0 * z**1.5d0) / (1.0d0 + 154.5d0 * z**3.5d0) + 
     *     (1.094d0 * capr2) / ((0.7813d9 - 1.234d5 * capr + 
     *     1201.0d0 * capr**1.5d0 + capr2) * (1.0d0 + 2.0d0 * y))
   
      p  = 1.8008d-7 * y4 / (1.0d0 + 0.0002863d0 * y4) - 
     *     2.121d0 * y2 / (794300.0d0 + y**4.3d0)
   
      q = 5.18d0 + 8.864d0 * y**3.5d0 / (3.788d6 + y4)
   
      overp = 10.47d0 / R**a + (b - bb) / R**c + 
     *               d * e / (1.0d0 + f * R**g) + 
     *               h + p / (R**q)
   
      overp = max(0.001d0,overp)

      return
      end
