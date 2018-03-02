c     this works now; it caused several problems:
c     (0) 'reset' collides with builtin
c     (1) variable reset was capitalized 
c     (2) passed function reset without backslash
      program test
      call fix
      stop '**BYE**'
      end
      function reset(i)
      print *, "HI!"
      reset=2
      return
      end
      subroutine fix
      call showme(reset(1))
      end
      subroutine showme(r)
      print *, 'showme',r
      end
