      block data au053
      common/dendat/rrd,rrb,rrc,rhod,rhoa,
     $r1i,r2i,r3i,r1ii,r2ii,r3ii,r2iii,r3iii
      data rrd,rrb,rrc,rhod,rhoa,
     $r1i,r2i,r3i,r1ii,r2ii,r3ii,r2iii,r3iii/
     $0.2878207442141723d+01,
     $0.3500000000000000d+01,
     $0.3900000000000000d+01,
     $0.1000000000000000d+01,
     $0.0000000000000000d+00,
     $-0.6800000000000000d+00,
     $0.7500000000000000d+00,
     $-0.1333333333333333D+01,
     $-0.6800000000000000D+00,
     $0.7500000000000000D+00,
     $-0.1527241171296038D+01,
     $0.5578188675490974D+01,
     $0.6132971688727435D+01/
      end
      program main
      common/dendat/rrd,rrb,rrc,rhod,rhoa,
     $r1i,r2i,r3i,r1ii,r2ii,r3ii,r2iii,r3iii
      print *, 'rrd, r1i, r3ii=',rrd,r1i,r3ii
      end
