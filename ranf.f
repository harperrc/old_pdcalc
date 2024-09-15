      function ranf ()  
c   
c  allow cdc compatible random number calls on the vax//convex  
c   
      include "real8.h"
      real*4 ran
      include "basicd.h"

      ranf = ran(iseed) 
      return

      end   
