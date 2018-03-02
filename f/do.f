c     different ways of writing do loops
c     look at the comma before the variable 'm'
c     all of these seem acceptable to g77 and f2c
      dimension a(10)
      do 1000, m=1,3
      a(m)=m
      print *,'loop 1000: m= ',m
1000  continue
      do 2000 m=4,7
      a(m)=m
      print *,'loop 2000: m= ',m
2000  continue
      do , m=8,10
      a(m)=m
      print *,'unlabeled loop: m= ',m
      enddo

c
c     'cycle' and 'exit' f77 extensions
      do n=1,5
      if (n .eq. 2) cycle
      if (n .eq.4) exit
      print *, 'cycle and exit test: ', n
      enddo

c     do while fortran extension
      i=0
      do while (i.lt.10) 
      i=i+1
      print *, 'in do while loop, i= ', i
      enddo
      end
