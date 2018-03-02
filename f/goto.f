        program goto
        call showsign(3)
        call showsign(-2)
        call showsign(0)
        call compgo
        end
        subroutine showsign(a)
        integer a

c       three-way goto
        if (a) 0100,200,300
 100    print *, a, ' is negative'
        return
00200   print *, a, ' is zero'
        return
 300    print *, a, ' is positive'
        return
        end

        subroutine compgo
      modtml=2
  205 go to (210,220,230,240,250) modtml
  210 print *, '210'
  220 print *, '220'
  230 print *, '230'
  240 print *, '240'
  250 print *, '250'
      return
      end

