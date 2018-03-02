c
c    this illustrates the need for prototypes in f2perl
c    g77 will run this ok; sub1 will print the first element of a
c    at present, sub2 will fail because a is passed as
c    an array, not a scalar
        dimension a(2,2)
        a(1,1)=1
        a(2,2)=4
        a(1,2)=3
        a(2,1)=3

c       Here f2perl passes 'a' as an array in all of these calls
c       which, in this case, is not correct for sub2.
        call sub1(a(1,1))
        call sub2(a(1,1))
        call sub1(a)
        call sub2(a)
        end
        subroutine sub1(a)
        dimension a(2,2)
        b=a(2,2)
        print *,'in sub1, a(2,2)=',b
        return
        end

        subroutine sub2(a)
        b=a
        print *,'in sub2, a(1,1)=',b
        return
        end
