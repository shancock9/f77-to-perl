c     note different lengths reported for the whole string
c     fortran reports the allocated string length
c     perl reports the non-blank string length
      character*40 string
      string='abcdefghijklmnop'
      i=len(string(5:10))
      j=len(string)
      print *, 'string=',string
      print *, 'len(5:10)= ', i, ' len(string)=', j
      end
