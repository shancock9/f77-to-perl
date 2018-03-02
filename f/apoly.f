c
c     a small program for testing f2perl:
c     includs a parameter, data, arrays, a subroutine call, a function call,
c     do loops, if statements, formatted output, and list-directed print
c     statements.
      parameter (ngon=4)
      dimension x(ngon), y(ngon)

c     data for an arbitrary polygon:
      data x/0,.5,1,.5/, y/.5,0,.5,1/

      print *,'Testing this polygon:'

c     list directed print      
      print *,'x(i)=',x
      print *,'y(i)=',y

c     find the bounding box using a subroutine call
      call bbox(ngon,x,y,xmin,xmax,ymin,ymax)

c     a formatted write
      write (*, 1000) xmin, xmax, ymin, ymax
 1000 format (1x,'xmin=',1pe12.4,1x,'xmax=',e12.4,1x,'ymin=',e12.4,1x,
     1 'ymax=',e12.4)

c     find the area using a function call
      area = apoly(ngon,x,y)

c     a free-format print
      print *,'area=',area
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
