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
       real (8) ak,bk,ck,dk,ek,fk
       integer i,j,k,l
       real (8) val1,val2,pfv1,pfv2,kin_part,overlap,pfc
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
          val1 = 0; val2 = 0;
          do k=3,3
            select case(k)
             case(1) !1234
              ak=a1;bk=b1;ck=c1;dk=d1;ek=e1;fk=f1
             case(2) !3214
              ak=d1;bk=b1;ck=f1;dk=a1;ek=e1;fk=c1 
             case(3) !3412
              ak=f1;bk=b1;ck=d1;d1=c1;ek=e1;fk=a1
             case(4) !1432
              ak=c1;bk=b1;ck=a1;d1=f1;ek=e1;fk=d1
             case(5) !2143
              ak=a1;bk=e1;ck=d1;d1=c1;ek=b1;fk=f1
             case(6) !4123
              ak=d1;bk=e1;ck=a1;d1=f1;ek=b1;fk=c1
             case(7) !4321
              ak=f1;bk=e1;ck=c1;d1=d1;ek=b1;fk=a1
             case(8) !2341
              ak=c1;bk=e1;ck=f1;d1=a1;ek=b1;fk=d1
            end select
  
            do l=3,3
            select case(l)
             case(1) !1234
              al=a2;bl=b2;cl=c2;dl=d2;el=e2;fl=f2
             case(2) !3214
              al=d2;bl=b2;cl=f2;dl=a2;el=e2;fl=c2 
             case(3) !3412
              al=f2;bl=b2;cl=d2;d2=c2;el=e2;fl=a2
             case(4) !1432
              al=c2;bl=b2;cl=a2;d2=f2;el=e2;fl=d2
             case(5) !2143
              al=a2;bl=e2;cl=d2;d2=c2;el=b2;fl=f2
             case(6) !4123
              al=d2;bl=e2;cl=a2;d2=f2;el=b2;fl=c2
             case(7) !4321
              al=f2;bl=e2;cl=c2;d2=d2;el=b2;fl=a2
             case(8) !2341
              al=c2;bl=e2;cl=f2;d2=a2;el=b2;fl=d2
            end select


          if (i.LE.(N*0.5)) then
c first Quadrant(u,u)
             if (j.LE.(N*0.5)) then
              val1=val1+kin_part(ak,bk,ck,dk,ek,fk,al,bl,cl,dl,el,fl) 
     -             + pfv1(ak,bk,ck,dk,ek,fk,al,bl,cl,dl,el,fl)
              val2=val2+overlap(ak,bk,ck,dk,ek,fk,al,bl,cl,dl,el,fl)
             else 
c third Quadrant(w,u)
             val2=0
              val1=val1+((-1)**(k+1))*pfc(ak,bk,ck,dk,ek,fk,al,bl,cl,dl,el,fl)
             endif 

          else 
c second Quadrant(u,w)
             if (j.LE.(N*0.5)) then
              val2=0
              val1=val1+((-1)**(l+1))*pfc(ak,bk,ck,dk,ek,fk,al,bl,cl,dl,el,fl)
            else 
c fourth Quadrant(w,w)
               val1=val1+((-1)**(k+l))*(kin_part(ak,bk,ck,dk,ek,fk,al,bl,cl,dl,el,fl)
     -              +pfv2(ak,bk,ck,dk,ek,fk,al,bl,cl,dl,el,fl))
               val2=val2+((-1)**(k+l))*overlap(ak,bk,ck,dk,ek,fk,al,bl,cl,dl,el,fl)
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


