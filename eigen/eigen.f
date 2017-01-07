*********1*********2*********3*********4*********5*********6*********7** 
      SUBROUTINE EIGEN(NN,X,F)
      USE TABLES
      IMPLICIT NONE
      integer NN
      real(8) F,X(NN)

      INTEGER I,J,S,T,K,l
      real (8) SUM1,SUM2,tau
      real (8) V(N),V0(N),VV(N),EE,EE2,TMP
      real (8) p(N),b(N),VHH(N),VDN(N),VHHOLD(N),VDNOLD(N),u(N),dd(N)
      LOGICAL sing

      INTEGER IPIV(N),ITMAX

      ITMAX=40

      if(updtbl.eq.0)then

c EP - initialized in main program

       CALL HAMLNORM(VHH,VDN,0)
       HHOLD = HH
       HH = HH - EP*DN

       call qrdcmp(HH,N,N,qc,d,sing)

       QT = 0.d0
       do i=1,N
        QT(i,i)=1.d0
       enddo

c u_j vector
c j-th element column of u_i
c k-th element QT(*,k)
       do j=1,N-1
        do k=1,N
         tau = 0.d0
         do i=j,N
          tau = tau + HH(i,j)*QT(i,k)
         enddo
         tau =tau/qc(j)
         do i=j,N
          QT(i,k) = QT(i,k) - tau*HH(i,j)
         enddo
        enddo
       enddo

       do i=1,N
        HH(i,i) = d(i)
       enddo

       do j=1,N-1
        do i=j+1,N
         HH(i,j)=0.d0
        enddo
       enddo
******************************************************************************************
      else 
       tmp = (X(4)*X(5) + X(3)*(X(4) + X(5)))*X(6) + X(2)*(X(5)*(X(4) + X(6)) + X(3)*(X(4) + X(5) + X(6))) + 
     -   X(1)*(X(4)*(X(5) + X(6)) + X(2)*(X(4) + X(5) + X(6)) + X(3)*(X(4) + X(5) + X(6)))

        if(tmp<=0.d0) then
         F = BSTE+0.1d0
         goto 100          
        endif

        if((X(1)<0.d0).or.(X(2)<0.d0).or.(X(3)<0.d0).or.(X(4)<0.d0).or.(X(5)<0.d0).or.(X(6)<0.d0))then
         F = BSTE+0.1d0
         goto 100          
        endif

        do i=1,6
         PHI(updtbl,i) = X(i)
        enddo

c update i-th row and column HHOLD,DNOLD
c determine update vectors to new QR decomposition

       CALL HAMLNORM(VHH,VDN,updtbl)
       do j=1,N
        VHHOLD(j) = HHOLD(updtbl,j)
        VDNOLD(j) = DN(updtbl,j)
        HHOLD(updtbl,j)=VHH(j)
        HHOLD(j,updtbl)=VHH(j)
        DN(updtbl,j)=VDN(j)
        DN(j,updtbl)=VDN(j)
        VHH(j) = VHH(j)-VHHOLD(j)
        VDN(j) = VDN(j)-VDNOLD(j)
       enddo        
       VHH(updtbl) = VHH(updtbl)/2
       VDN(updtbl) = VDN(updtbl)/2
       VHH = VHH - EP*VDN
       V =  VHH
       U = 0.d0
       U(updtbl) = 1.d0        
       U = MATMUL(QT,U)
       call qrupdt(HH,QT,N,N,u,v)
       U = 0.d0
       U(updtbl) = 1.d0        
       V = MATMUL(QT,VHH)
       call qrupdt(HH,QT,N,N,v,u)

*********************************************************************8

      EE2 = EP

      endif

 110  continue

      V = 1.d0
      V0 = MATMUL(DN,V)
      DO I =1,ITMAX
       V  = V0
       call qrsolv2(HH,QT,N,N,V)
       SUM1 = DOT_PRODUCT(V,V0)
       V0 = MATMUL(DN,V)
       SUM2 = DOT_PRODUCT(V,V0)       
       EE   = EP+SUM1/SUM2
c       write(*,*) i,EE
       SUM2 = 1/SQRT(SUM2)
       V0 = V0*SUM2
       
       if(ABS((EE-EE2)/EE)<1.d-16) goto 120
       
       EE2 = EE
      ENDDO
      
 120  continue
     
      F = EE 

c      write(*,*) updtbl(-1),' F :: ',F

c  NORMALIZATION 
       SUM1 = DOT_PRODUCT(V,MATMUL(DN,V))
       SUM1 = 1/SQRT(SUM1)
       V = SUM1*V

c current best solution
      if(BSTE>F)then
       BSTE = F
       BSTI = updtbl
       write(*,*) updtbl,' Best E : ',BSTE, '       Delta E : ' ,-BSTE-2*((NC**2-1)/(4*NC))**2

       BPHI = PHI
       do i=1,N
        BPHI(i,7)=V(i)
       enddo

       if(foutWF.eq.1) call outWF(PHI,V,out_fileWF)
      endif

 100  continue
       return
      END  
*********1*********2*********3*********4*********5*********6*********7** 
