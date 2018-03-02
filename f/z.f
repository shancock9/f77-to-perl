c
c    octal and hex values
         DATA IFAC/'177777'O/
         ILIM = Z'0000FEDD'
         print *,ilim
         print 1000,ilim
 1000    format(1x,':',Z8,1x,':')
         print *,ifac
         end
