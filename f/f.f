      subroutine p_fS(f,sm,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
      USE PRMTS
      IMPLICIT NONE
       integer sm,idx6,i,j
       real (8) a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2,f(0:MAXF)
       real (8) overlap,kin_part
       f(idx6(0,0,0,0,0,0)) = overlap(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
       do i =1,8
          do j =1,8
          enddo
       enddo


c$$$     -            overlap(a1,b1,c1,d1,e1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            overlap(a1,b1,c1,d1,e1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            overlap(a1,b1,c1,d1,e1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            overlap(a1,b1,c1,d1,e1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            overlap(a1,b1,c1,d1,e1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            overlap(a1,b1,c1,d1,e1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            overlap(a1,b1,c1,d1,e1,f1,f2,e2,c2,d2,b2,a2)+     
c$$$     -            overlap(a1,d1,e1,b1,c1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            overlap(a1,d1,e1,b1,c1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            overlap(a1,d1,e1,b1,c1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            overlap(a1,d1,e1,b1,c1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            overlap(a1,d1,e1,b1,c1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            overlap(a1,d1,e1,b1,c1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            overlap(a1,d1,e1,b1,c1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            overlap(a1,d1,e1,b1,c1,f1,f2,e2,c2,d2,b2,a2)+               
c$$$     -            overlap(a1,c1,b1,e1,d1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            overlap(a1,c1,b1,e1,d1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            overlap(a1,c1,b1,e1,d1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            overlap(a1,c1,b1,e1,d1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            overlap(a1,c1,b1,e1,d1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            overlap(a1,c1,b1,e1,d1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            overlap(a1,c1,b1,e1,d1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            overlap(a1,c1,b1,e1,d1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            overlap(a1,e1,d1,c1,b1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            overlap(a1,e1,d1,c1,b1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            overlap(a1,e1,d1,c1,b1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            overlap(a1,e1,d1,c1,b1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            overlap(a1,e1,d1,c1,b1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            overlap(a1,e1,d1,c1,b1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            overlap(a1,e1,d1,c1,b1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            overlap(a1,e1,d1,c1,b1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            overlap(f1,b1,d1,c1,e1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            overlap(f1,b1,d1,c1,e1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            overlap(f1,b1,d1,c1,e1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            overlap(f1,b1,d1,c1,e1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            overlap(f1,b1,d1,c1,e1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            overlap(f1,b1,d1,c1,e1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            overlap(f1,b1,d1,c1,e1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            overlap(f1,b1,d1,c1,e1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            overlap(f1,d1,b1,e1,c1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            overlap(f1,d1,b1,e1,c1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            overlap(f1,d1,b1,e1,c1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            overlap(f1,d1,b1,e1,c1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            overlap(f1,d1,b1,e1,c1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            overlap(f1,d1,b1,e1,c1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            overlap(f1,d1,b1,e1,c1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            overlap(f1,d1,b1,e1,c1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            overlap(f1,c1,e1,b1,d1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            overlap(f1,c1,e1,b1,d1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            overlap(f1,c1,e1,b1,d1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            overlap(f1,c1,e1,b1,d1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            overlap(f1,c1,e1,b1,d1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            overlap(f1,c1,e1,b1,d1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            overlap(f1,c1,e1,b1,d1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            overlap(f1,c1,e1,b1,d1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            overlap(f1,e1,c1,d1,b1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            overlap(f1,e1,c1,d1,b1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            overlap(f1,e1,c1,d1,b1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            overlap(f1,e1,c1,d1,b1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            overlap(f1,e1,c1,d1,b1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            overlap(f1,e1,c1,d1,b1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            overlap(f1,e1,c1,d1,b1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            overlap(f1,e1,c1,d1,b1,a1,f2,e2,c2,d2,b2,a2)

 
     	f(idx6(0,0,0,0,0,2)) = kin_part(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
c$$$     -            kin_part(a1,b1,c1,d1,e1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            kin_part(a1,b1,c1,d1,e1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            kin_part(a1,b1,c1,d1,e1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            kin_part(a1,b1,c1,d1,e1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            kin_part(a1,b1,c1,d1,e1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            kin_part(a1,b1,c1,d1,e1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            kin_part(a1,b1,c1,d1,e1,f1,f2,e2,c2,d2,b2,a2)+     
c$$$     -            kin_part(a1,d1,e1,b1,c1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            kin_part(a1,d1,e1,b1,c1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            kin_part(a1,d1,e1,b1,c1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            kin_part(a1,d1,e1,b1,c1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            kin_part(a1,d1,e1,b1,c1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            kin_part(a1,d1,e1,b1,c1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            kin_part(a1,d1,e1,b1,c1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            kin_part(a1,d1,e1,b1,c1,f1,f2,e2,c2,d2,b2,a2)+               
c$$$     -            kin_part(a1,c1,b1,e1,d1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            kin_part(a1,c1,b1,e1,d1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            kin_part(a1,c1,b1,e1,d1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            kin_part(a1,c1,b1,e1,d1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            kin_part(a1,c1,b1,e1,d1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            kin_part(a1,c1,b1,e1,d1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            kin_part(a1,c1,b1,e1,d1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            kin_part(a1,c1,b1,e1,d1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            kin_part(a1,e1,d1,c1,b1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            kin_part(a1,e1,d1,c1,b1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            kin_part(a1,e1,d1,c1,b1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            kin_part(a1,e1,d1,c1,b1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            kin_part(a1,e1,d1,c1,b1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            kin_part(a1,e1,d1,c1,b1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            kin_part(a1,e1,d1,c1,b1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            kin_part(a1,e1,d1,c1,b1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            kin_part(f1,b1,d1,c1,e1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            kin_part(f1,b1,d1,c1,e1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            kin_part(f1,b1,d1,c1,e1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            kin_part(f1,b1,d1,c1,e1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            kin_part(f1,b1,d1,c1,e1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            kin_part(f1,b1,d1,c1,e1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            kin_part(f1,b1,d1,c1,e1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            kin_part(f1,b1,d1,c1,e1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            kin_part(f1,d1,b1,e1,c1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            kin_part(f1,d1,b1,e1,c1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            kin_part(f1,d1,b1,e1,c1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            kin_part(f1,d1,b1,e1,c1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            kin_part(f1,d1,b1,e1,c1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            kin_part(f1,d1,b1,e1,c1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            kin_part(f1,d1,b1,e1,c1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            kin_part(f1,d1,b1,e1,c1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            kin_part(f1,c1,e1,b1,d1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            kin_part(f1,c1,e1,b1,d1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            kin_part(f1,c1,e1,b1,d1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            kin_part(f1,c1,e1,b1,d1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            kin_part(f1,c1,e1,b1,d1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            kin_part(f1,c1,e1,b1,d1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            kin_part(f1,c1,e1,b1,d1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            kin_part(f1,c1,e1,b1,d1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            kin_part(f1,e1,c1,d1,b1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            kin_part(f1,e1,c1,d1,b1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            kin_part(f1,e1,c1,d1,b1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            kin_part(f1,e1,c1,d1,b1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            kin_part(f1,e1,c1,d1,b1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            kin_part(f1,e1,c1,d1,b1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            kin_part(f1,e1,c1,d1,b1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            kin_part(f1,e1,c1,d1,b1,a1,f2,e2,c2,d2,b2,a2)
                
       


 100   continue

 10    FORMAT(I3,I3,I3,I3,I3,I3)
 20    FORMAT(I3,I3,I3,I3,I3,I3,F20.16)

        return
      end subroutine p_fS

*************************************************************************
      subroutine p_fV(f,sm,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
      USE PRMTS
      IMPLICIT NONE
      integer sm,idx6
      real (8) a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2,f(0:MAXF)
      real (8) pfv1,pfv2
      
      f(idx6(0,0,0,0,0,-1)) = pfv1(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
c$$$     -            pfv1(a1,b1,c1,d1,e1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv1(a1,b1,c1,d1,e1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv1(a1,b1,c1,d1,e1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv1(a1,b1,c1,d1,e1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv1(a1,b1,c1,d1,e1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv1(a1,b1,c1,d1,e1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv1(a1,b1,c1,d1,e1,f1,f2,e2,c2,d2,b2,a2)+     
c$$$     -            pfv1(a1,d1,e1,b1,c1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv1(a1,d1,e1,b1,c1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv1(a1,d1,e1,b1,c1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv1(a1,d1,e1,b1,c1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv1(a1,d1,e1,b1,c1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv1(a1,d1,e1,b1,c1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv1(a1,d1,e1,b1,c1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv1(a1,d1,e1,b1,c1,f1,f2,e2,c2,d2,b2,a2)+               
c$$$     -            pfv1(a1,c1,b1,e1,d1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv1(a1,c1,b1,e1,d1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv1(a1,c1,b1,e1,d1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv1(a1,c1,b1,e1,d1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv1(a1,c1,b1,e1,d1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv1(a1,c1,b1,e1,d1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv1(a1,c1,b1,e1,d1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv1(a1,c1,b1,e1,d1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv1(a1,e1,d1,c1,b1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv1(a1,e1,d1,c1,b1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv1(a1,e1,d1,c1,b1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv1(a1,e1,d1,c1,b1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv1(a1,e1,d1,c1,b1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv1(a1,e1,d1,c1,b1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv1(a1,e1,d1,c1,b1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv1(a1,e1,d1,c1,b1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv1(f1,b1,d1,c1,e1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv1(f1,b1,d1,c1,e1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv1(f1,b1,d1,c1,e1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv1(f1,b1,d1,c1,e1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv1(f1,b1,d1,c1,e1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv1(f1,b1,d1,c1,e1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv1(f1,b1,d1,c1,e1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv1(f1,b1,d1,c1,e1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv1(f1,d1,b1,e1,c1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv1(f1,d1,b1,e1,c1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv1(f1,d1,b1,e1,c1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv1(f1,d1,b1,e1,c1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv1(f1,d1,b1,e1,c1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv1(f1,d1,b1,e1,c1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv1(f1,d1,b1,e1,c1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv1(f1,d1,b1,e1,c1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv1(f1,c1,e1,b1,d1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv1(f1,c1,e1,b1,d1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv1(f1,c1,e1,b1,d1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv1(f1,c1,e1,b1,d1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv1(f1,c1,e1,b1,d1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv1(f1,c1,e1,b1,d1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv1(f1,c1,e1,b1,d1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv1(f1,c1,e1,b1,d1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv1(f1,e1,c1,d1,b1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv1(f1,e1,c1,d1,b1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv1(f1,e1,c1,d1,b1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv1(f1,e1,c1,d1,b1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv1(f1,e1,c1,d1,b1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv1(f1,e1,c1,d1,b1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv1(f1,e1,c1,d1,b1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv1(f1,e1,c1,d1,b1,a1,f2,e2,c2,d2,b2,a2)


      f(idx6(0,0,0,0,-1,0)) = pfv2(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)    
c$$$     -            pfv2(a1,b1,c1,d1,e1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv2(a1,b1,c1,d1,e1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv2(a1,b1,c1,d1,e1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv2(a1,b1,c1,d1,e1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv2(a1,b1,c1,d1,e1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv2(a1,b1,c1,d1,e1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv2(a1,b1,c1,d1,e1,f1,f2,e2,c2,d2,b2,a2)+     
c$$$     -            pfv2(a1,d1,e1,b1,c1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv2(a1,d1,e1,b1,c1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv2(a1,d1,e1,b1,c1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv2(a1,d1,e1,b1,c1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv2(a1,d1,e1,b1,c1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv2(a1,d1,e1,b1,c1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv2(a1,d1,e1,b1,c1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv2(a1,d1,e1,b1,c1,f1,f2,e2,c2,d2,b2,a2)+               
c$$$     -            pfv2(a1,c1,b1,e1,d1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv2(a1,c1,b1,e1,d1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv2(a1,c1,b1,e1,d1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv2(a1,c1,b1,e1,d1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv2(a1,c1,b1,e1,d1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv2(a1,c1,b1,e1,d1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv2(a1,c1,b1,e1,d1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv2(a1,c1,b1,e1,d1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv2(a1,e1,d1,c1,b1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv2(a1,e1,d1,c1,b1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv2(a1,e1,d1,c1,b1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv2(a1,e1,d1,c1,b1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv2(a1,e1,d1,c1,b1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv2(a1,e1,d1,c1,b1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv2(a1,e1,d1,c1,b1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv2(a1,e1,d1,c1,b1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv2(f1,b1,d1,c1,e1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv2(f1,b1,d1,c1,e1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv2(f1,b1,d1,c1,e1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv2(f1,b1,d1,c1,e1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv2(f1,b1,d1,c1,e1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv2(f1,b1,d1,c1,e1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv2(f1,b1,d1,c1,e1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv2(f1,b1,d1,c1,e1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv2(f1,d1,b1,e1,c1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv2(f1,d1,b1,e1,c1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv2(f1,d1,b1,e1,c1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv2(f1,d1,b1,e1,c1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv2(f1,d1,b1,e1,c1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv2(f1,d1,b1,e1,c1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv2(f1,d1,b1,e1,c1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv2(f1,d1,b1,e1,c1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv2(f1,c1,e1,b1,d1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv2(f1,c1,e1,b1,d1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv2(f1,c1,e1,b1,d1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv2(f1,c1,e1,b1,d1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv2(f1,c1,e1,b1,d1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv2(f1,c1,e1,b1,d1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv2(f1,c1,e1,b1,d1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv2(f1,c1,e1,b1,d1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfv2(f1,e1,c1,d1,b1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfv2(f1,e1,c1,d1,b1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfv2(f1,e1,c1,d1,b1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfv2(f1,e1,c1,d1,b1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfv2(f1,e1,c1,d1,b1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfv2(f1,e1,c1,d1,b1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfv2(f1,e1,c1,d1,b1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfv2(f1,e1,c1,d1,b1,a1,f2,e2,c2,d2,b2,a2)




 100   continue
 10    FORMAT(I3,I3,I3,I3,I3,I3)
 20    FORMAT(I3,I3,I3,I3,I3,I3,F30.10)
        return
      end subroutine p_fv


*************************************************************************
      subroutine p_fC(f,sm,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
      USE PRMTS
      IMPLICIT NONE
      integer sm,idx6
      real (8) a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2,f(0:MAXF)
      real (8) pfc
      	f(idx6(0,0,1,0,0,0)) = pfc(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
c$$$     -            pfc(a1,b1,c1,d1,e1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfc(a1,b1,c1,d1,e1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfc(a1,b1,c1,d1,e1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfc(a1,b1,c1,d1,e1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfc(a1,b1,c1,d1,e1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfc(a1,b1,c1,d1,e1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfc(a1,b1,c1,d1,e1,f1,f2,e2,c2,d2,b2,a2)+     
c$$$     -            pfc(a1,d1,e1,b1,c1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfc(a1,d1,e1,b1,c1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfc(a1,d1,e1,b1,c1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfc(a1,d1,e1,b1,c1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfc(a1,d1,e1,b1,c1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfc(a1,d1,e1,b1,c1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfc(a1,d1,e1,b1,c1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfc(a1,d1,e1,b1,c1,f1,f2,e2,c2,d2,b2,a2)+               
c$$$     -            pfc(a1,c1,b1,e1,d1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfc(a1,c1,b1,e1,d1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfc(a1,c1,b1,e1,d1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfc(a1,c1,b1,e1,d1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfc(a1,c1,b1,e1,d1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfc(a1,c1,b1,e1,d1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfc(a1,c1,b1,e1,d1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfc(a1,c1,b1,e1,d1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfc(a1,e1,d1,c1,b1,f1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfc(a1,e1,d1,c1,b1,f1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfc(a1,e1,d1,c1,b1,f1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfc(a1,e1,d1,c1,b1,f1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfc(a1,e1,d1,c1,b1,f1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfc(a1,e1,d1,c1,b1,f1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfc(a1,e1,d1,c1,b1,f1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfc(a1,e1,d1,c1,b1,f1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfc(f1,b1,d1,c1,e1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfc(f1,b1,d1,c1,e1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfc(f1,b1,d1,c1,e1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfc(f1,b1,d1,c1,e1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfc(f1,b1,d1,c1,e1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfc(f1,b1,d1,c1,e1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfc(f1,b1,d1,c1,e1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfc(f1,b1,d1,c1,e1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfc(f1,d1,b1,e1,c1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfc(f1,d1,b1,e1,c1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfc(f1,d1,b1,e1,c1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfc(f1,d1,b1,e1,c1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfc(f1,d1,b1,e1,c1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfc(f1,d1,b1,e1,c1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfc(f1,d1,b1,e1,c1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfc(f1,d1,b1,e1,c1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfc(f1,c1,e1,b1,d1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfc(f1,c1,e1,b1,d1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfc(f1,c1,e1,b1,d1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfc(f1,c1,e1,b1,d1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfc(f1,c1,e1,b1,d1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfc(f1,c1,e1,b1,d1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfc(f1,c1,e1,b1,d1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfc(f1,c1,e1,b1,d1,a1,f2,e2,c2,d2,b2,a2)+
c$$$     -            pfc(f1,e1,c1,d1,b1,a1,a2,b2,c2,d2,e2,f2)+
c$$$     -            pfc(f1,e1,c1,d1,b1,a1,a2,d2,e2,b2,c2,f2)+
c$$$     -            pfc(f1,e1,c1,d1,b1,a1,a2,c2,b2,e2,d2,f2)+
c$$$     -            pfc(f1,e1,c1,d1,b1,a1,a2,e2,d2,c2,b2,f2)+
c$$$     -            pfc(f1,e1,c1,d1,b1,a1,f2,b2,d2,c2,e2,a2)+
c$$$     -            pfc(f1,e1,c1,d1,b1,a1,f2,d2,b2,e2,c2,a2)+
c$$$     -            pfc(f1,e1,c1,d1,b1,a1,f2,c2,e2,b2,d2,a2)+
c$$$     -            pfc(f1,e1,c1,d1,b1,a1,f2,e2,c2,d2,b2,a2)
 100   continue
 10    FORMAT(I3,I3,I3,I3,I3,I3)
 20    FORMAT(I3,I3,I3,I3,I3,I3,F30.10)
        return
      end subroutine p_fC
*************************************************************************
      real(8) function PF1(a,b,c,d,e,f)
      implicit none
      real(8) a,b,c,d,e,f
       PF1=a*b*c + a*b*e + a*b*f + a*c*d + a*c*f + a*d*e + a*d*f + a*e*f + b*c*d + b*c*e + b*d*e + b*d*f + b*e*f + c*d*e + c*d*f + c*e*f
      return
      end function
*************************************************************************
      real(8) function PF2(a,b,c,d,e,f)
      implicit none
      real(8) a,b,c,d,e,f
      PF2=a*b + a*d + a*f + b*d + b*e + d*e + d*f + e*f
      return
      end function
*************************************************************************
      real(8) function F3(a,b,c,d,e,f)
      implicit none
      real(8) a,b,c,d,e,f
      F3=2.0*a*a + a*b + a*d - b*d + a*c + a*e - c*e
      return
      end function
*************************************************************************
      real(8) function overlap(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
      use prmts
      implicit none
      real(8) a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2
      real(8) a,b,c,d,e,f,F1temp,PF1
       a=a1+a2;b=b1+b2;c=c1+c2;d=d1+d2;e=e1+e2;f=f1+f2
       F1temp=PF1(a,b,c,d,e,f)
       overlap=1.0/(F1temp*sqrt(F1temp))
      return 
      end function
*************************************************************************
      real(8) function Fr2(a,b,c,d,e,f)
      implicit none
      real(8) a,b,c,d,e,f,F1temp,PF1,PF2
      F1temp=PF1(a,b,c,d,e,f)
      Fr2=3.0*PF2(a,b,c,d,e,f)/(2.0*F1temp*F1temp*sqrt(F1temp))
      return
      end function
*************************************************************************


      real(8) function Frinv(a,b,c,d,e,f)
      use prmts
      implicit none
      real(8) a,b,c,d,e,f,PF1,PF2
      Frinv=2.0/(PF1(a,b,c,d,e,f)*sqrt(pi*PF2(a,b,c,d,e,f)))
      return
      end function
*************************************************************************
       real(8) function kin_part(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
       use prmts
       implicit none
       real(8) a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2
       real(8) a,b,c,d,e,f,overlap,Fr2,F3,sum,T12,T13,T14,T23,T24,T34
        a=a1+a2;b=b1+b2;c=c1+c2;d=d1+d2;e=e1+e2;f=f1+f2
        sum=a1+b1+c1+d1+e1+f1
        T12=F3(a1,b1,c1,d1,e1,f1)*Fr2(c,b,a,f,e,d)
        T13=F3(b1,a1,c1,d1,f1,e1)*Fr2(a,c,b,e,d,f)
        T14=F3(c1,b1,a1,f1,e1,d1)*Fr2(a,b,c,d,e,f)
        T23=F3(d1,a1,e1,b1,f1,c1)*Fr2(a,e,d,c,b,f)
        T24=F3(e1,a1,d1,c1,f1,b1)*Fr2(a,d,e,b,c,f)
        T34=F3(f1,b1,d1,c1,e1,a1)*Fr2(b,d,f,a,c,e)
        kin_part=6.0*sum*overlap(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)-2*(T12+T13+T14+T23+T24+T34)
        
       return
       end function
*************************************************************************
*************************************************************************
      real(8) function exact_part(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
      use prmts
      implicit none
      real(8) a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2
      real(8) a,b,c,d,e,f,Frinv
      a=a1+a2;b=b1+b2;c=c1+c2;d=d1+d2;e=e1+e2;f=f1+f2
      exact_part=Frinv(c,b,a,f,e,d) + Frinv(b,d,f,a,c,e) - Frinv(a,c,b,e,d,f)- Frinv(a,b,c,d,e,f) - Frinv(a,e,d,c,b,f) - Frinv(a,d,e,b,c,f)

c v12+v34-v13-v14-v23-v24         
      return
      end function
*************************************************************************
      real(8) function pfv1(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
      use prmts
      use tables
      implicit none
      real(8) a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2
      real(8) a,b,c,de,e,f,exact_part
      real(8) Frinv,Fsuper,pd12,pd13,pd14,pd23,pd24,pd34
      a=a1+a2;b=b1+b2;c=c1+c2;de=d1+d2;e=e1+e2;f=f1+f2
      pd12=Frinv(c,b,a,f,e,de)
      pd34=Frinv(b,de,f,a,c,e)
      pd13=Frinv(a,c,b,e,de,f)
      pd14=Frinv(a,b,c,de,e,f)
      pd23=Frinv(a,e,de,c,b,f)
      pd24=Frinv(a,de,e,b,c,f)
      pfv1=(NC**2-1)*(pd12+pd34+pd14+pd23)/NC-(NC-1)*(2*pd13+2*pd24-pd23-pd34-pd12)/NC
      pfv1=pfv1*(-0.25)
c      pfv1=exact_part(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
      end function

*************************************************************************
      real(8) function pfv2(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
      use prmts
      use tables
      implicit none
      real(8) a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2
      real(8) a,b,c,de,e,f
      real(8) Frinv,pd12,pd13,pd14,pd23,pd24,pd34,exact_part
      a=a1+a2;b=b1+b2;c=c1+c2;de=d1+d2;e=e1+e2;f=f1+f2
      pd12=Frinv(c,b,a,f,e,de)
      pd34=Frinv(b,de,f,a,c,e)
      pd13=Frinv(a,c,b,e,de,f)
      pd14=Frinv(a,b,c,de,e,f)
      pd23=Frinv(a,e,de,c,b,f)
      pd24=Frinv(a,de,e,b,c,f)
      pfv2=(NC**2-1)*(pd12+pd34+pd14+pd23)/NC+(NC+1)*(2*pd13+2*pd24-pd23-pd34-pd12)/NC
      pfv2=pfv2*(-0.25)
c      pfv2=exact_part(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)*0.1
      return
      end function

*************************************************************************
      real(8) function pfc(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
      use prmts
      use tables
      implicit none
      real(8) a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2
      real(8) a,b,c,de,e,f
      real(8) pd12,pd34,pd14,pd23,Frinv
      a=a1+a2;b=b1+b2;c=c1+c2;de=d1+d2;e=e1+e2;f=f1+f2
      pd12=Frinv(c,b,a,f,e,de)
      pd34=Frinv(b,de,f,a,c,e)
      pd14=Frinv(a,b,c,de,e,f)
      pd23=Frinv(a,e,de,c,b,f)
      pfc=sqrt(NC**2-1)*(pd12+pd34-pd14-pd23)
      pfc=pfc*(-0.25)
c      pfc=0
      return 
      end function

