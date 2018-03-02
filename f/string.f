      dimension string(2)
      character*50 string
      string(1)='abcdefg'
      string(2)='hijklmnop'
      string(1)(2:3)='yz'
      string(2)(5:6)='56'
      print *, string
      end
