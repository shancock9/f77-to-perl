c
c    Some loops and string manipulations. All of this can be done in
c    a couple of lines of perl code, but its a good test of f2perl.
c    This will generate some warnings
c    about the len function and .eq. operator, but it works as is.
        character*40 string
        data string/'    Just Another Perl Hacker '/
        print *, 'Initial String: ',string
        call ljust(string)
        print *, 'Left Justified: ',string
        call locase(string)
        print *, 'Lower Cased   : ',string
        end
c
      integer function first (string)
*
*     function FIRST returns an integer that points to the first
*     non-blank character in the character variable STRING.
      character*(*)   string
*
*     find last non-blank character in STRING
      lpmax = len(string)
      do 100 lp = 1,lpmax,1
      ich=ichar(string(lp:lp))
      if(string(lp:lp) .eq. ' ' .or. ich.eq.9)  go to 100
      first = lp
      go to 900
  100 continue
      first = lpmax
*
*     termination for function FIRST
  900 return
      end

c     NOTE that function name 'last' is a perl keyword and will
c     be capitalized
      integer function last (string)
*
*     function last returns an integer that points to the last
*     non-blank character in the character variable string.
      character*(*)   string
*
*     find last non-blank character in string
      lpmax = len(string)
c
c     Note negative do loop index
      do 100 lp = lpmax,1,-1
      if(string(lp:lp) .eq. ' ')  go to 100
      last = lp
      go to 900
  100 continue
      last = 0
*
*     termination for function last
  900 return
      end
        subroutine ljust(string)
c
c       left justify a string
        character*(*) string
        integer first, last
c
c       find indexes locb:loce of the non-blank portion of the string
        locb=first(string)
        if (locb.le.1) go to 900
        loce=last(string)
        n=1
        do i=locb,loce
                string(n:n)=string(i:i)
                n=n+1
        enddo
        do k=n,loce
                string(k:k)=' '
        enddo
  900   return
        end
        subroutine locase (str)
c
c       convert string to lower case
        character*(*) str
        data initlz/0/, idif/0/
        if (initlz.eq.0) then
                initlz=1
                idif=ichar('A')-ichar('a')
        endif
        lmax=last(str)
        if (lmax.gt.0) then
         do 100 l=1,lmax
         if (.not.(llt(str(l:l),'A') .or. lgt(str(l:l),'Z'))) then
           str(l:l)= char(ichar(str(l:l))-idif)
         endif
  100   continue
        endif
        return
        end
