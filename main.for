      program main
c
      character*132 dummy

      id = 0

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
      else if (id.eq.4) then
         call drv4
      else if (id.eq.5) then
         call drv5
      else if (id.eq.6) then
         call drv6
      else if (id.eq.7) then
         call drv7
      else if (id.eq.8) then
         call drv8
      else
         write(6,*)'no driver ',id
      endif

      stop
      end
      include 'drv1.x'
      include 'drv2.x'
      include 'drv3.x'
      include 'drv4.x'
      include 'drv5.x'
      include 'drv6.x'
      include 'drv7.x'
      include 'drv8.x'
