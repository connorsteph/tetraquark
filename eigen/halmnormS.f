      SUBROUTINE HAMLNORM(VHH,VDN,upidx)
       USE TABLES
       USE PRMTS
       IMPLICIT NONE

c update vectors
   
       real (8) VHH(N),VDN(N)
       integer upidx,idxmin,idxmax
       real (8) a1,b1,c1,d1,e1,f1
       real (8) a2,b2,c2,d2,e2,f2
       integer i,j,s
       real (8) C(8),Z,val1,val2,kin,pot1,pot2,dig
       real (8) f(0:MAXF)
       integer idx6,sm

***************************************************************
       
       if(upidx.eq.0)then
        idxmin = 1
        idxmax = N
        do i=1,N
         do j=1,N
          HH(i,j)=0.d0; DN(i,j)=0.d0
         enddo
        enddo
       else 
        idxmin=upidx
        idxmax=upidx
         do j=1,N
          VHH(j)=0.d0; VDN(j)=0.d0
         enddo
       endif

       C(1)=1;C(2)=1;C(3)=1;C(4)=1;C(5)=1;C(6)=1;C(7)=1;C(8)=1
        do i=idxmin,idxmax
         a1 = ABS(PHI(i,1))
         b1 = ABS(PHI(i,2))
         c1 = ABS(PHI(i,3))
         d1 = ABS(PHI(i,4))
         e1 = ABS(PHI(i,5))
         f1 = ABS(PHI(i,6))
         do j=1,N
          a2 = ABS(PHI(j,1))
          b2 = ABS(PHI(j,2))
          c2 = ABS(PHI(j,3))
          d2 = ABS(PHI(j,4))
          e2 = ABS(PHI(j,5))
          f2 = ABS(PHI(j,6))
          

          if (i.LE.(N*0.5)) then
c first Quadrant(s,s)
             if (j.LE.(N*0.5)) then
              call p_fS(f,sm,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
              call p_fV(f,sm,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
              val1=f(idx6(0,0,0,0,0,2))+f(idx6(0,0,0,0,0,-1))
              val2=f(idx6(0,0,0,0,0,0))
             else 
c third Quadrant(d,s)
             val2=0
              call p_fC(f,sm,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
              val1=f(idx6(0,0,1,0,0,0))
             endif 

          else 
c second Quadrant(s,d)
             if (j.LE.(N*0.5)) then
              val2=0
              call p_fC(f,sm,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
              val1=f(idx6(0,0,1,0,0,0))
            else 
               call p_fS(f,sm,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
               call p_fV(f,sm,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
               val1=f(idx6(0,0,0,0,0,2))+f(idx6(0,0,0,0,-1,0))
               val2=f(idx6(0,0,0,0,0,0))
             endif  
c fourth Quadrant(d,d)
          endif

        
        if(upidx.eq.0)then
         HH(i,j)=HH(i,j)+val1
         DN(i,j)=DN(i,j)+val2
        else
         VHH(j)=VHH(j)+val1
         VDN(j)=VDN(j)+val2 
        endif


 100   continue
            
        enddo
       enddo

      RETURN
      END


