      program main
c
      character*132 dummy

      id = 1

      if (iargc().gt.0) then
         call getarg(1,dummy)
         read(dummy,*)id
      endif

      if (id.eq.1) then
         call drv1
      else if (id.eq.2) then
         call drv2
      else if (id.eq.3) then
         call drv3
      endif

      stop
      end
      include 'drv1.x'
      include 'drv2.x'
      include 'drv3.x'
