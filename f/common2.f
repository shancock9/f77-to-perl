c
c       two common blocks in one line:
        common /a/ b, c /d/ e, f
        e=2.2
        call check
        end
        subroutine check
        common /d/ e, f
        print *,'e=',e
        end
