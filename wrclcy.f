      subroutine wrclcy(kf,yield,wr,ierr)

c  compute weapon radius for special crated type targets
c
c  inputs
c     kf      which crater weapon radius equation to use
c             (1-17)
c     yield   weapon yield (kt)

c  output
c     wr      weapon radius (feet)
c     ierr    error status

      include "real8.h"
      include "const.h"
      include "files.h"

      dimension ycof(17),yexp(17)

      data ycof/ 28.0d0 , 89.0d0 ,131.0d0 ,136.0d0 ,140.0d0 ,141.0d0,
     *          146.0d0 ,148.0d0 ,155.0d0 ,185.0d0 ,209.0d0 ,214.0d0,
     *          219.0d0 ,229.0d0 ,230.0d0 ,231.0d0 ,232.0d0/

      data yexp/0.546d0 ,0.381d0 ,0.352d0 ,0.357d0 ,0.324d0 ,0.323d0,
     *          0.323d0 ,0.325d0 ,0.375d0 ,0.367d0 ,0.333d0 ,0.338d0,
     *          0.334d0 ,0.311d0 ,0.321d0 ,0.310d0 ,0.316d0/

      ierr = 0

      if(kf.lt.1.or.kf.gt.17) then
         ierr=7
         return
      endif

c  compute weapon radius

      wr = ycof(kf) * yield**yexp(kf)

      return
      end
