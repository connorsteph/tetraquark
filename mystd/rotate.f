      SUBROUTINE rotate(r,qt,n,np,i,a,b)
      IMPLICIT NONE
      INTEGER n,np,i
      DOUBLE PRECISION a,b,r(np,np),qt(np,np)
      INTEGER j
      DOUBLE PRECISION c,fact,s,w,y
      if(a.eq.0.d0)then
        c=0.d0
        s=sign(1.d0,b)
      else if(abs(a).gt.abs(b))then
        fact=b/a
        c=sign(1.d0/sqrt(1.d0+fact**2),a)
        s=fact*c
      else
        fact=a/b
        s=sign(1.d0/sqrt(1.d0+fact**2),b)
        c=fact*s
      endif
      do 11 j=i,n
        y=r(i,j)
        w=r(i+1,j)
        r(i,j)=c*y-s*w
        r(i+1,j)=s*y+c*w
11    continue
      do 12 j=1,n
        y=qt(i,j)
        w=qt(i+1,j)
        qt(i,j)=c*y-s*w
        qt(i+1,j)=s*y+c*w
12    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Vs94z&):9+X%1j49#:`*.
