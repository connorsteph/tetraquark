      SUBROUTINE qrdcmp(a,n,np,c,d,sing)
      IMPLICIT NONE
      INTEGER n,np
      DOUBLE PRECISION a(np,np),c(n),d(n)
      LOGICAL sing
      INTEGER i,j,k
      DOUBLE PRECISION scale,sigma,sum,tau
      sing=.false.
      scale=0.d0
      do 17 k=1,n-1
        do 11 i=k,n
          scale=max(scale,abs(a(i,k)))
11      continue
        if(scale.eq.0.d0)then
          sing=.true.
          c(k)=0.d0
          d(k)=0.d0
        else
          do 12 i=k,n
            a(i,k)=a(i,k)/scale
12        continue
          sum=0.d0
          do 13 i=k,n
            sum=sum+a(i,k)**2
13        continue
          sigma=sign(sqrt(sum),a(k,k))
          a(k,k)=a(k,k)+sigma
          c(k)=sigma*a(k,k)
          d(k)=-scale*sigma
          do 16 j=k+1,n
            sum=0.d0
            do 14 i=k,n
              sum=sum+a(i,k)*a(i,j)
14          continue
            tau=sum/c(k)
            do 15 i=k,n
              a(i,j)=a(i,j)-tau*a(i,k)
15          continue
16        continue
        endif
17    continue
      d(n)=a(n,n)
      if(d(n).eq.0.d0)sing=.true.
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Vs94z&):9+X%1j49#:`*.
