      SUBROUTINE HAMLNORM(VHH,VDN,upidx)
       USE TABLES
       USE PRMTS
       IMPLICIT NONE

c update vectors
   
       real (8) VHH(N),VDN(N)
       integer upidx,idxmin,idxmax
       real (8) a1,b1,c1,d1,e1,f1
       real (8) a2,b2,c2,d2,e2,f2
       real (8) al,bl,cl,dl,el,fl
       real (8) am,bm,cm,dm,em,fm
       integer i,j,l,m
       real (8) C(8),Z,val1,val2,kin,pot1,pot2,dig
       real (8) f(0:MAXF)
       integer idx6

***************************************************************
       val1 = 0.0
       val2 = 0.0
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

       do i=idxmin,idxmax
         a1 = (PHI(i,1))
         b1 = (PHI(i,2))
         c1 = (PHI(i,3))
         d1 = (PHI(i,4))
         e1 = (PHI(i,5))
         f1 = (PHI(i,6))
         do j=1,N
          a2 = (PHI(j,1))
          b2 = (PHI(j,2))
          c2 = (PHI(j,3))
          d2 = (PHI(j,4))
          e2 = (PHI(j,5))
          f2 = (PHI(j,6))
          
          do l = 1,8
            select case(l)
             case(1) !1234
              al=a1;bl=b1;cl=c1;dl=d1;el=e1;fl=f1
             case(2) !2134
              al=d1;bl=e1;cl=a1;dl=f1;el=b1;fl=c1 
             case(3) !1243
              al=f1;bl=b1;cl=d1;d1=c1;el=e1;fl=a1
             case(4) !2143
              al=c1;bl=e1;cl=f1;d1=a1;el=b1;fl=d1
             case(5) !3412
              al=f1;bl=e1;cl=c1;d1=d1;el=b1;fl=a1
             case(6) !4312
              al=d1;bl=b1;cl=f1;d1=a1;el=e1;fl=c1
             case(7) !3421
              al=a1;bl=e1;cl=d1;d1=c1;el=b1;fl=f1
             case(8) !4321
              al=c1;bl=b1;cl=a1;d1=f1;el=e1;fl=d1
            end select
  
            do m =1,8
            select case(m)
             case(1) !1234
              am=a1;bm=b1;cm=c1;dm=d1;em=e1;fm=f1
             case(2) !2134
              am=d1;bm=e1;cm=a1;dm=f1;em=b1;fm=c1 
             case(3) !1243
              am=f1;bm=b1;cm=d1;d1=c1;em=e1;fm=a1
             case(4) !2143
              am=c1;bm=e1;cm=f1;d1=a1;em=b1;fm=d1
             case(5) !3412
              am=f1;bm=e1;cm=c1;d1=d1;em=b1;fm=a1
             case(6) !4312
              am=d1;bm=b1;cm=f1;d1=a1;em=e1;fm=c1
             case(7) !3421
              am=a1;bm=e1;cm=d1;d1=c1;em=b1;fm=f1
             case(8) !4321
              am=c1;bm=b1;cm=a1;d1=f1;em=e1;fm=d1
            end select


          if (i.LE.(N*0.5)) then
c first Quadrant(u,u)
             if (j.LE.(N*0.5)) then
c              call p_fS(f,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
c              call p_fV(f,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
c              val1=f(idx6(0,0,0,0,0,2))+f(idx6(0,0,0,0,0,-1))
c              val2=f(idx6(0,0,0,0,0,0))
              val1=val1+kin_part(al,bl,cl,dl,el,fl,am,bm,cm,dm,em,fm) + pfv1(al,bl,cl,dl,el,fl,am,bm,cm,dm,em,fm)
              val2=val2+overlap(al,bl,cl,dl,el,fl,am,bm,cm,dm,em,fm)
             else 
c third Quadrant(w,u)
             val2=0
c              call p_fC(f,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
c              val1=f(idx6(0,0,1,0,0,0))
              val1=val1+((-1)**(i+1))*pfc(al,bl,cl,dl,el,fl,am,bm,cm,dm,em,fm)
             endif 

          else 
c second Quadrant(u,w)
             if (j.LE.(N*0.5)) then
              val2=0
              call p_fC(f,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
c              val1=f(idx6(0,0,1,0,0,0))
              val1=val1+((-1)**(j+1))*pfc(al,bl,cl,dl,el,fl,am,bm,cm,dm,em,fm)
            else 
c fourth Quadrant(w,w)
c               call p_fS(f,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
c               call p_fV(f,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)
c               val1=f(idx6(0,0,0,0,0,2))+f(idx6(0,0,0,0,-1,0))
c               val2=f(idx6(0,0,0,0,0,0))
               val1=val1+kin_part(al,bl,cl,dl,el,fl,am,bm,cm,dm,em,fm)+pfv2(al,bl,cl,dl,el,fl,am,bm,cm,dm,em,fm)
               val2=val2+((-1)**(i+j))*overlap(al,bl,cl,dl,el,fl,am,bm,cm,dm,em,fm)
             endif               
          endif
          enddo 
          enddo
        
        if(upidx.eq.0)then
         HH(i,j)=val1
         DN(i,j)=val2
        else
         VHH(j)=val1
         VDN(j)=val2 
        endif


 100   continue
            
        enddo
       enddo

      RETURN
      END


