c
c     Example of testing for points in a polygon
      parameter (ngon=4)
      dimension x(ngon), y(ngon)
      data x/0,.5,1,.5/, y/.5,0,.5,1/
      print *,'Testing this polygon:'
      print *,'x(i)=',x
      print *,'y(i)=',y

c     First find the area and bounding box
      call bbox(ngon,x,y,xmin,xmax,ymin,ymax)
      area = apoly(ngon,x,y)
      print *,'xmin,ymin,xmax,ymax,area=',xmin,xmax,ymin,ymax,area
      print *,' '

c     Now loop to test a lot of points with this polygon
      xt=-.45
      yt=-.45
      do i=1,10
        xt=xt+.2
        yt=yt+.2
        inside=inpoly (xt,yt,ngon,x,y,xmin,xmax,ymin,ymax,area)
        print *,'x=',xt,' y=',yt,' inpoly:',inside
      enddo
      stop
      end

      subroutine bbox(ngon,xpoly,ypoly,xmin,xmax,ymin,ymax)
c
c     given a polygon (xpoly(i),ypoly(i),i=1,ngon)
c     compute the bounding box xmin,xmax,ymin,ymax
      dimension xpoly(1), ypoly(1)
      xmin=xpoly(1)
      xmax=xpoly(1)
      ymin=ypoly(1)
      ymax=ypoly(1)
      do n=2,ngon
        if (xpoly(n) .lt. xmin) xmin=xpoly(n)
        if (ypoly(n) .lt. ymin) ymin=ypoly(n)
        if (xpoly(n) .gt. xmax) xmax=xpoly(n)
        if (ypoly(n) .gt. ymax) ymax=ypoly(n)
      enddo
      return
      end

      function apoly(ngon,xpoly,ypoly)
c
c     given a polygon (xpoly(i),ypoly(i),i=1,ngon)
c     returns polygon area.  
      dimension xpoly(1), ypoly(1)
      apoly=0.0
      x1=xpoly(1)
      y1=ypoly(1)
      do n=3,ngon
        dx1 = xpoly(n-1)-x1
        dy1 = ypoly(n-1)-y1
        dx2 = xpoly(n)  -x1
        dy2 = ypoly(n)  -y1
        apoly=apoly+dx1*dy2-dx2*dy1
      enddo
      apoly = apoly/2.
      return
      end

      function inpoly (xt,yt,ngon,x,y,xmin,xmax,ymin,ymax,area)
c
c     ******************************************************************
c
c     Test point (xt,yt) lies in the polygon with vertexes 
c           (x(n),y(n),n=1,...,ngon
c
c     The inside of the polygon is the region on the left as we travel
c     around the polygon from point n to point n+1.   So, polygons
c     with negative area are inverted from the normal sense.
c
c     The polygon should be non-self-intersecting.
c
c     If the polygon is not closed, a straight line is assumed
c     to connect the last point with the first point.
c
c     Returns:
c       0 if xt,yt is not in the polygon
c       1 if xt,yt it is in the polygon
c
c     ******************************************************************
c
      dimension x(ngon),y(ngon)
      inpoly=0
      if (xt .ge. xmin .and. xt .le. xmax .and.
     1    yt .ge. ymin .and. yt .le. ymax) then
        inpoly = jordan(xt,yt,ngon,x,y)
      endif
      if (area .lt. 0) inpoly=1-inpoly
      return
      end
      
      function jordan (xt,yt,ngon,x,y)
c
c     ******************************************************************
c
c     jordan determines if test point (xt,yt) lies in the polygon with 
c     vertexes (x(n),y(n),n=1,...,ngon.  It returns
c       0 if xt,yt is not in the polygon
c       1 if xt,yt it is in the polygon
c
c     Method: use Jordan's theorem in the plane, in which we draw a
c     line from the test point and count the number of intersections.  
c     If the number is even, the point is out; if odd, the point is in.
c
c     If the polygon is not closed, a straight line is assumed
c     to connect the last point with the first point.
c
c     Steve Hancock, March 1996
c
c     ******************************************************************
c
      dimension x(ngon),y(ngon)
      logical yge, inside
c
      inside = .false.
      jordan = 0
      nm = ngon
      yge = y(nm).ge.yt
c
c     loop over all polygon segments
      do n=1,ngon
c
c     check for a crossing at y=yt
      if (yge .neqv. (y(n).ge.yt)) then
c
c       we have a level crossing, now check x location
        yge=.not.yge
        if (n.ne.1) nm=n-1
c
c       the intersection of this line with the +x ray is given by:
c              xx=(x(nm)+(x(n)-x(nm))*((yt-y(nm))/(y(n)-y(nm)))
c       and we want to count a crossing if xx >= xt.  Therefore, we
c       count if:
c               xx.gt.xt
c
c       subtract x(nm) from both sides, and then multiply both sides
c       by y(n)-y(nm), which has the sign of yge, to get this test:
c
        if( ((x(n)-x(nm))*(yt-y(nm)) .ge. (xt-x(nm))*(y(n)-y(nm)))
     1      .eqv. yge ) inside=.not.inside
      endif
      enddo
      if (inside) jordan=1
c
c     termination for subroutine jordan
      return 
      end
