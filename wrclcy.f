      subroutine wrclcy(kf,yield,wr,ierr)
c
c  compute weapon radius for special crated type targets
c
c  inputs
c     kf      which crater weapon radius equation to use
c             (1-17)
c     yield   weapon yield (kt)
c
c  output
c     wr      weapon radius (feet)
c     ierr    error status
c
      include "real8.h"
      include "const.h"
      include "files.h"

      dimension ycof(17),yexp(17)
c
      data ycof/28.,89.,131.,136.,140.,141.,146.,148.,155.,185.,
     *  209.,214.,219.,229.,230.,231.,232./
      data yexp/.546,.381,.352,.357,.324,.323,.323,.325,.375,.367,
     *  .333,.338,.334,.311,.321,.310,.316/
c
      ierr=0
      if(kf.lt.1.or.kf.gt.17) then
         ierr=7
         return
      endif
c
c  compute weapon radius
c
      wr=ycof(kf)*yield**yexp(kf)
c
      return
      end
