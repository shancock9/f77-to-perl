c
c  integer divide tests
c  f2perl will catch this first one
       dimension cb(2)
       cb(1)=3.14
       xc=5.667
       cw=2.22
       mcx=(xc-cb(1))/cw+0.99999
       print * , 'mcx=',mcx
c
c  f2perl does not do this second one yet:
       b=3.3
       a=b+7/3
       print *,'a=',a,' b=',b
       end
