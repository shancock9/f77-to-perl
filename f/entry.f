c     simple test of entry statement
      call pos
      call neg
      stop '**BYE**'
      end
      subroutine sign
      entry pos
         print *,'positive'
      return
      entry neg 
         print *,'negative'
      return
      end
