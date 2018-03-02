c
c     print to a format string created at run time
c     this will invoke 'use Fortran::ToPerl::Format;'
      character*40 fmt
      fmt='(1x,5h.....)'
      fmt(7:11)='XYZZY'
      print fmt
      stop
      end
