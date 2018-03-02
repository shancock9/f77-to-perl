        dimension a(1), b(7), e(2,3)
        equivalence (a(1), b(2))
        equivalence (b(4), e(2,3))
        equivalence (b(7),q(3,4))
        dimension x(4), y(2)
        equivalence (x(1), y(1))
        dimension q(3,4)
        common /ii/ q
        do n=1,7
         b(n)=n
        enddo
        print * , 'a(1)=',a(1)
        print * , 'e(2,3)=',e(2,3)
        print * , 'q(2,4)=',q(2,4)
        print * , 'q(3,4)=',q(3,4)
        end
