c
c   testing the index function
c   note that we have to give the range of string2 or this wont work
        character*40 string1, string2
        string1='monkey see monkey do'
        string2='key'
        i=index(string1,string2(1:3))
        j=index(string2(1:3),string1)
        print *,'i=',i,' j=',j
        end
