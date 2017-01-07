      SUBROUTINE qrsolv(a,n,np,c,d,b)
      IMPLICIT NONE
      INTEGER n,np
      real (8) a(np,np),b(n),c(n),d(n)
CU    USES rsolv
      INTEGER i,j
      real (8) sum,tau
      do 13 j=1,n-1
        sum=0.d0
        do 11 i=j,n
          sum=sum+a(i,j)*b(i)
11      continue
        tau=sum/c(j)
        do 12 i=j,n
          b(i)=b(i)-tau*a(i,j)
12      continue
13    continue
      call rsolv(a,n,np,d,b)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Vs94z&):9+X%1j49#:`*.


      SUBROUTINE qrsolv2(r,qt,n,np,b)
      IMPLICIT NONE
      INTEGER n,np
      real (8) r(np,np),qt(np,np),b(n)
      INTEGER i,j
      real (8) b2(n),sum,tau

      do i=1,N
       b2(i)=0.d0
       do j=1,N
        b2(i) = b2(i) + qt(i,j)*b(j) 
       enddo
      enddo
      b = b2

      b(n) = b(n)/r(n,n)
      do i=n-1,1,-1
       sum=0.d0
       do j=i+1,n
        sum=sum+r(i,j)*b(j)
       enddo
       b(i)=(b(i)-sum)/r(i,i)
      enddo

      return
      END

