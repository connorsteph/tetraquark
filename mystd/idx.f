cccccccccccccccccccccccccccccccccccccccccccccccccccc
c    idx6(n1,n2,n3,n4,n5,n6) -> N;

      integer  function idx6(n1,n2,n3,n4,n5,n6)
      IMPLICIT NONE
      integer sum
      integer n1,n2,n3,n4,n5,n6,nx

       nx  = n1+1
       sum = nx+1
       nx  = nx+n2+1
       sum = sum + (nx*(1 + nx))/2
       nx  = nx+n3+1
       sum = sum + (nx*(1 + nx)*(2 + nx))/6
       nx  = nx+n4+1
       sum = sum + (nx*(1 + nx)*(2 + nx)*(3 + nx))/24
       nx  = nx+n5+1
       sum = sum + (nx*(1 + nx)*(2 + nx)*(3 + nx)*(4 + nx))/120
       nx  = nx+n6+1
       sum = sum + nx*(1 + nx)/2*(2 + nx)/3*(3 + nx)/2*(4 + nx)*(5 + nx)/60

c      write (*,*) (nx*(1 + nx)*(2 + nx)*(3 + nx)*(4 + nx)*(5 + nx))/720

       if (sum<0) then
        idx6 = 0
        return
       endif

       idx6 = sum       
       return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccc
      real (8) function delta(n)
      IMPLICIT NONE
       integer n
       if (n==0) then
        delta=1.d0
       else
        delta=0.d0
       endif
       return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccc
