      SUBROUTINE HAMLNORM(VHH,VDN,upidx)
      USE TABLES
      USE PRMTS
      IMPLICIT NONE

      real (8) VHH(N),VDN(N)
      integer upidx,idxmin,idxmax
      real (8) a1,b1,c1,d1,e1,f1
      real (8) a2,b2,c2,d2,e2,f2
      real (8) ar,br,cr,dr,er,fr
      real (8) aq,bq,cq,dq,eq,fq
      integer i,j,q,r
      real (8) val1,val2,pfv1,pfv2,kin_part,overlap,pfc
      real (8) sgn_ui,sgn_uj,sgn_wi,sgn_wj
***************************************************************
c     if(upidx.eq.0)then
      if(0.eq.0)then
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

c     do i=idxmin,idxmax
      do i = 1,N
         a1 = PHI(i,1)
         b1 = PHI(i,2)
         c1 = PHI(i,3)
         d1 = PHI(i,4)
         e1 = PHI(i,5)
         f1 = PHI(i,6)
         do j=1,N
            a2 = PHI(j,1)
            b2 = PHI(j,2)
            c2 = PHI(j,3)
            d2 = PHI(j,4)
            e2 = PHI(j,5)
            f2 = PHI(j,6)

            val1 = 0; val2 = 0;
c     sgn_ui & sgn_wi here are the parities of the the transformation that creates
c     each term in the various wavefunction symmetries. 


c     Currently the wavefunction is u<+->
c$$$            do q=1,8
c$$$               select case(q)
c$$$            case(1)             !1234
c$$$               aq=a1;bq=b1;cq=c1;dq=d1;eq=e1;fq=f1;sgn_ui=1.0;sgn_wi=1.0
c$$$            case(2)             !3214
c$$$               aq=d1;bq=b1;cq=f1;dq=a1;eq=e1;fq=c1;sgn_ui=1.0;sgn_wi=-1.0 
c$$$            case(3)             !3412
c$$$               aq=f1;bq=b1;cq=d1;dq=c1;eq=e1;fq=a1;sgn_ui=-1.0;sgn_wi=-1.0
c$$$            case(4)             !1432
c$$$               aq=c1;bq=b1;cq=a1;dq=f1;eq=e1;fq=d1;sgn_ui=-1.0;sgn_wi=1.0
c$$$            case(5)             !2143
c$$$               aq=a1;bq=e1;cq=d1;dq=c1;eq=b1;fq=f1;sgn_ui=1.0;sgn_wi=1.0
c$$$            case(6)             !4123
c$$$               aq=c1;bq=e1;cq=f1;dq=a1;eq=b1;fq=d1;sgn_ui=1.0;sgn_wi=-1.0
c$$$            case(7)             !4321
c$$$               aq=f1;bq=e1;cq=c1;dq=d1;eq=b1;fq=a1;sgn_ui=-1.0;sgn_wi=-1.0
c$$$            case(8)             !2341
c$$$               aq=d1;bq=e1;cq=a1;dq=f1;eq=b1;fq=c1;sgn_ui=-1.0;sgn_wi=1.0
c$$$            end select
c$$$            
c$$$            do r=1,8
c$$$              select case(r)
c$$$            case(1)             !1234
c$$$               ar=a2;br=b2;cr=c2;dr=d2;er=e2;fr=f2;sgn_uj=1.0;sgn_wj=1.0
c$$$            case(2)             !3214
c$$$               ar=d2;br=b2;cr=f2;dr=a2;er=e2;fr=c2;sgn_uj=1.0;sgn_wj=-1.0 
c$$$            case(3)             !3412
c$$$               ar=f2;br=b2;cr=d2;dr=c2;er=e2;fr=a2;sgn_uj=-1.0;sgn_wj=-1.0
c$$$            case(4)             !1432
c$$$               ar=c2;br=b2;cr=a2;dr=f2;er=e2;fr=d2;sgn_uj=-1.0;sgn_wj=1.0
c$$$            case(5)             !2143
c$$$               ar=a2;br=e2;cr=d2;dr=c2;er=b2;fr=f2;sgn_uj=1.0;sgn_wj=1.0
c$$$            case(6)             !4123
c$$$               ar=c2;br=e2;cr=f2;dr=a2;er=b2;fr=d2;sgn_uj=1.0;sgn_wj=-1.0
c$$$            case(7)             !4321
c$$$               ar=f2;br=e2;cr=c2;dr=d2;er=b2;fr=a2;sgn_uj=-1.0;sgn_wj=-1.0
c$$$            case(8)             !2341
c$$$               ar=d2;br=e2;cr=a2;dr=f2;er=b2;fr=c2;sgn_uj=-1.0;sgn_wj=1.0
c$$$            End select

c     u<++>
            do q=1,1
               select case(q)
            case(1)             !1234
               aq=a1;bq=b1;cq=c1;dq=d1;eq=e1;fq=f1;sgn_ui=1.0;sgn_wi=1.0
            case(2)             !3214
               aq=d1;bq=b1;cq=f1;dq=a1;eq=e1;fq=c1;sgn_ui=1.0;sgn_wi=-1.0 
            case(3)             !3412
               aq=f1;bq=b1;cq=d1;dq=c1;eq=e1;fq=a1;sgn_ui=1.0;sgn_wi=1.0
            case(4)             !1432
               aq=c1;bq=b1;cq=a1;dq=f1;eq=e1;fq=d1;sgn_ui=1.0;sgn_wi=-1.0
            case(5)             !2143
               aq=a1;bq=e1;cq=d1;dq=c1;eq=b1;fq=f1;sgn_ui=1.0;sgn_wi=1.0
            case(6)             !4123
               aq=c1;bq=e1;cq=f1;dq=a1;eq=b1;fq=d1;sgn_ui=1.0;sgn_wi=-1.0
            case(7)             !4321
               aq=f1;bq=e1;cq=c1;dq=d1;eq=b1;fq=a1;sgn_ui=1.0;sgn_wi=1.0
            case(8)             !2341
               aq=d1;bq=e1;cq=a1;dq=f1;eq=b1;fq=c1;sgn_ui=1.0;sgn_wi=-1.0
            end select
            
            do r=1,1
              select case(r)
            case(1)             !1234
               ar=a2;br=b2;cr=c2;dr=d2;er=e2;fr=f2;sgn_uj=1.0;sgn_wj=1.0
            case(2)             !3214
               ar=d2;br=b2;cr=f2;dr=a2;er=e2;fr=c2;sgn_uj=1.0;sgn_wj=-1.0 
            case(3)             !3412
               ar=f2;br=b2;cr=d2;dr=c2;er=e2;fr=a2;sgn_uj=1.0;sgn_wj=1.0
            case(4)             !1432
               ar=c2;br=b2;cr=a2;dr=f2;er=e2;fr=d2;sgn_uj=1.0;sgn_wj=-1.0
            case(5)             !2143
               ar=a2;br=e2;cr=d2;dr=c2;er=b2;fr=f2;sgn_uj=1.0;sgn_wj=1.0
            case(6)             !4123
               ar=c2;br=e2;cr=f2;dr=a2;er=b2;fr=d2;sgn_uj=1.0;sgn_wj=-1.0
            case(7)             !4321
               ar=f2;br=e2;cr=c2;dr=d2;er=b2;fr=a2;sgn_uj=1.0;sgn_wj=1.0
            case(8)             !2341
               ar=d2;br=e2;cr=a2;dr=f2;er=b2;fr=c2;sgn_uj=1.0;sgn_wj=-1.0
            End select





            if (i.LE.(N*0.5)) then
c     upper left quadrant(u,u)
               if (j.LE.(N*0.5)) then
                  val1=val1+sgn_wi*sgn_wj*(kin_part(aq,bq,cq,dq,eq,fq,ar,br,cr,dr,er,fr) 
     -                 + pfv1(aq,bq,cq,dq,eq,fq,ar,br,cr,dr,er,fr))
                  val2=val2+sgn_wi*sgn_wj*overlap(aq,bq,cq,dq,eq,fq,ar,br,cr,dr,er,fr)
               else 
c     upper right quadrant(u,w)
                  val2=0
                  val1=val1+sgn_wi*sgn_uj*pfc(aq,bq,cq,dq,eq,fq,ar,br,cr,dr,er,fr)
               endif 

            else 
c     lower left quadrant(w,u)
               if (j.LE.(N*0.5)) then
                  val2=0
                  val1=val1+sgn_ui*sgn_wj*pfc(aq,bq,cq,dq,eq,fq,ar,br,cr,dr,er,fr)
               else 
c     lower right quadrant(w,w)
                  val1=val1+sgn_ui*sgn_uj*(kin_part(aq,bq,cq,dq,eq,fq,ar,br,cr,dr,er,fr)
     -                 +pfv2(aq,bq,cq,dq,eq,fq,ar,br,cr,dr,er,fr))
                  val2=val2+sgn_ui*sgn_uj*overlap(aq,bq,cq,dq,eq,fq,ar,br,cr,dr,er,fr)
               endif               
            endif
            
         enddo 
      enddo
      
c     if(upidx.eq.0)then
      if(0.eq.0)then
         HH(i,j)=val1
         DN(i,j)=val2
      else
         VHH(j)=val1
         VDN(j)=val2 
      endif
      
      enddo
      enddo
      RETURN
      END


