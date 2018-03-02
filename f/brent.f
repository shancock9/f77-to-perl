c
c       Use Brent's procedure ZERO to find a root
c       The function fofx contains the function whose root is sought
c       This version looks for the root of sin(x)+x-1=0
        dimension bvec(7) 
        data tol/0.000001/,maxit/25/ 
c
c       find bounds for the root
        sa=0
        fa=fofx(sa)
        sb=1
        fb=fofx(sb)
c
c      normally we would check that the root is bounded by sa and sb,
c      and keep looking for a bound if necessary.  But in this case
c      the above values are known to bound the root.
        if (fa*fb .ge. 0) then
           print *, 'sa=',sa,'fa=',fa,'sb=',sb,'fb=',fb
           stop 'Bad starting states'
        endif

c     initialize for a search
        call nbrenti (sa,fa,sb,fb,tol,xx,bvec) 
  
c     loop over iterations 
        do 250 iter=1,maxit 
  
      ff = fofx(xx) 
  
c     get next value of xx 
      call nbrentx (ff,xx,ifconv,bvec) 
      if (ifconv.ne.0) go to 700 
  
c       note - it is possible to make a convergence test here on the 
c       magnitude of ff as well 
  
  250 continue 
  
c     no convergence after maxit iterations -- cant happen if maxit 
c               is large enough for the given tol - see brents book 
  
c     converged - finish up 
  700 check = fofx(xx)  
      print *, 'last x is ', xx, ' f(x)= ',check
      stop '**BYE**'
      end

c
c     This is the function whose root we seek.
c     This version looks for the root of x+sin(x)=1
      function fofx(x)
      fofx = x+sin(x)-1
      return
      end

        subroutine nbrenti(sa,fa,sb,fb,tol,xnext,bvec)
c
c       initialization for brents root finder
c       (see-algorithms for minimization without
c       derivatives-,by richard p. brent, prentice-hall, 1973.)
c
c       input parameters -
c       bounding states:
c         sa - first x coordinate
c         fa = f(sa)
c         sb - second x coordinate
c         fb = f(sb)
c         **  must have fa*fb <= 0. **
c       tolx = convergence tolerance on x
c
c       output parameter -
c       xnext = next value of x to try
c
c       there are two subroutines:
c               brenti must be called once to start an interation
c               brentx must be called once for each step
c
c****************************************************************
c
c       sample coding: assume we have a root trapped between sa & sb
c
c       dimension bvec(7)
c       data tol/.001/,maxit/25/
c
c     call nbrenti (sa,fa,sb,fb,tol,xx,bvec)
c
c     loop over iterations
c     do 250 iter=1,maxit
c
c       calculate ff(xx)
c
c       get next value of xx
c     call nbrentx (ff,xx,ifconv,bvec)
c     if (ifconv.ne.0) go to 700
c
c       note - it is possible to make a convergence test here on the
c       magnitude of ff as well
c
c 250 continue
c
c     no convergence after maxit iterations -- cant happen if maxit
c               is large enough for the given tol - see brents book
c
c     converged - finish up
c 700 ...
c****************************************************************
c
        dimension bvec(7)
        sc = sb
        fc = fb
        bvec(1) = sa
        bvec(2) = fa
        bvec(3) = sb
        bvec(4) = fb
        bvec(5) = sc
        bvec(6) = fc
        bvec(7) = tol
        call nbrentx(fb,xnext,ifconv,bvec)
        return
        end
      subroutine nbrentx (flast,xnext,ifconv,bvec)
c
c     take one iteration step with brents method
c     flast = value of f of previous step
c     xnext = next value of x to try
c     ifconv = 0 if not converged
c            = 1 if converged
c       bvec = state vector
c
        dimension bvec(7)
c
c       get the previous state
        sa = bvec(1)
        fa = bvec(2)
        sb = bvec(3)
        fb = bvec(4)
        sc = bvec(5)
        fc = bvec(6)
        tol = bvec(7)
c
      fb = flast
      if ((fb.gt.0.) .and. (fc.gt.0.)) go to 210
      if ((fb.le.0.) .and. (fc.le.0.)) go to 210
      go to 220
c
  210 sc = sa
      fc = fa
      be = sb-sa
      bd = be
  220 if(abs(fc).ge.abs(fb)) go to 230
      sa = sb
      sb = sc
      sc = sa
      fa = fb
      fb = fc
      fc = fa
  230 bm = 0.5*(sc-sb)
      if ((abs(bm).le.tol) .or. (fb.eq.0.)) go to 340
      if ((abs(be).ge.tol) .and. (abs(fa).gt.abs(fb))) go to 240
c
c         bisection is forced
      be = bm
      bd = be
      go to 300
c
  240 bs = fb/fa
      if(sa.ne.sc) go to 250
c
c         linear interpolation
      bp = 2.*bm*bs
      bq = 1.-bs
      go to 260
c
c         inverse quadratic interpolation
  250 bq = fa/fc
      br = fb/fc
      bp = bs*(2.*bm*bq*(bq-br)-(sb-sa)*(br-1.))
      bq = (bq-1.)*(br-1.)*(bs-1.)
c
  260 if(bp.le.0.) go to 270
      bq = -bq
      go to 280
  270 bp = -bp
  280 bs = be
      be = bd
      if((2.*bp.ge.3.*bm*bq-abs(tol*bq)).or.(bp.ge.abs(0.5*bs*bq)))
     1go to 290
      bd = bp/bq
      go to 300
  290 be = bm
      bd = be
  300 sa = sb
      fa = fb
      if (abs(bd).le.tol) go to 310
      sb = sb+bd
      go to 330
  310 if(bm.le.0.) go to 320
      sb = sb + tol
      go to 330
  320 sb = sb - tol
  330 xnext = sb
      ifconv = 0
      go to 900
  340 ifconv = 1
c
c       save the state
  900   bvec(1) = sa
        bvec(2) = fa
        bvec(3) = sb
        bvec(4) = fb
        bvec(5) = sc
        bvec(6) = fc
      return
      end
