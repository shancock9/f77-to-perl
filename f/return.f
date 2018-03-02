c     alternate returns
      program test
      CALL ENTERB(*700,*450,T5)
 700  print *, 'RETURNED 1'
      return
 450  print *, 'RETURNED 2'
      return
      end
      SUBROUTINE ENTERB(*,*,T6)
      RETURN 2
      END
