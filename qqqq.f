C***********************************************************************
      USE TABLES
      USE PRMTS
      IMPLICIT NONE
      real (8) F
      integer nfcc,j,k
      real (8), DIMENSION(:), ALLOCATABLE :: EE,X
      CHARACTER( LEN =30 ) :: in_file

      real (8) oldF 

****************initial parameters *******************
      in_file="qqqq.init"
      call ininit(in_file)
**********************************************
      call inWF(in_fileWF,resume)
      if (newN.gt.N) then
         k = N+1
      else
         k=1
      endif
      N=newN
****************optimization *******************
      BSTE = 0.d0
      BPHI = PHI 
      updtbl = 0

c     Set EP to 2*quarkonium energy
      EP = -2*((NC**2-1)/(4*NC))**2+EP
      call EIGEN(X,F)
      
      write(*,*) F
c     Regenerate random parameters and recalculate initial energy if the matrix singular (and thus energy is NaN)
      do while(F.ne.F)
         call inWF(in_fileWF,resume)
         call eigen(X,F)
      end do

      oldF = F
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ALLOCATE(X(NN))
      ALLOCATE(EE(NN))

c     Begin optimization, iterating through basis function 1,N

      write(*,*) 'Optimize ..... '
      do while (myacc.ne.0.0)

         if(k.eq.1) then 
            write(*,*) 'multEE: ',multEE,'myacc: ',myacc
         endif
         
c     how many elements to update
         updtbl = k

         do j=1,NN
            EE(j) = 1.d-1*multEE*Sqrt(PHI(k,j))
            X(j) = PHI(k,j)
         enddo

         call VA04AD(X,EE,NN,F,ESCALE,IPRINT,ICON,MAXIT,nfcc)

         call BSYNCHRONIZE(X,F)

         k = MOD(k,N) + 1

         if((mod(k,200).eq.1).or.((k-BSTI).eq.20)) then 
            BSTI = k
            updtbl = 0
            PHI = BPHI
            call eigen(X,F)
         endif

         if(k.eq.1) then 
            write(*,*) 'oldF,F: ',oldF,F,oldF-F
            if(ABS(oldF-F) < myacc) then
               write(*,*) 'Accuracy changed'
               multEE = multEE*0.5d0 
               myacc = myacc*0.5d0
            endif
            oldF = F
         endif

      enddo

 100  continue

      END

*********1*********2*********3*********4*********5*********6*********7** 

      SUBROUTINE BSYNCHRONIZE(X,F)
      USE TABLES
      use prmts
      IMPLICIT NONE
      real(8) F,X(NN)
      
      integer i 
      logical flag
      
      flag=.false.
      do i=1,6
         if (PHI(updtbl,i)<>BPHI(updtbl,i)) flag = .true.
      enddo

      if(flag)then
         do i=1,6 
            X(i)=BPHI(updtbl,i)       
         enddo
         call eigen(X,F)
      endif

      RETURN
      END

*********1*********2*********3*********4*********5*********6*********7** 
      
