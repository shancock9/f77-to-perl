c
c     Test local, multiple dimensioned array
c     This can become a perl array since it is not in common and
c     not passed as a call parameter.
      parameter(imax=2,jmax=3,kmax=4,lmax=5)
      dimension a(imax,jmax,kmax,lmax)
      do i=1,imax
      do j=1,jmax
      do k=1,kmax
      do l=1,lmax
      a(i,j,k,l)=i+j+k+l
      enddo
      enddo
      enddo
      enddo
      i=1
      j=2
      k=3
      do l=1,lmax
      b=a(i,j,k,l)
      print *, 'a(',i,',',j,',',k,',',l,')=',b
      enddo
      end
