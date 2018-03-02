c
c       Data statement for a common variable -
c       g77 can handle this, but f2perl will write
c       an error message to use BLOCKDATA for 'b' and 'e'.
        common /a/ b, c
        data b/34.0/
        common /a/ e
        data e/36.7/
        print *,'b=',b, 'e=',e
        end
