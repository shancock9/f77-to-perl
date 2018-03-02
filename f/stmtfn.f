c
c       test a statement function
        call sub1(4,k)
        print *, 'k=',k
        end
        subroutine sub1(i,k)

c       j is a statement function
        j(i)=i+1
        k=j(2*i)
        return 
        end
