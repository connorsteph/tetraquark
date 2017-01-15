       SUBROUTINE inWF(in_file,switch)
       USE TABLES
       IMPLICIT NONE

       CHARACTER( LEN = 30 ) :: in_file
       integer i,j,switch
       real(8) b,a,dev,energy,old_NC
       WRITE(*,*) 'Readind file : ', in_file
       OPEN(UNIT=9,FILE=in_file,FORM='UNFORMATTED')
       b=3d-2;a=1d-9
       read(9) N
       read(9) energy
       read(9) old_NC
       write(*,*) 'N: ', N
       write(*,*) 'newN ',newN
       write(*,*) 'Previous best energy: ', energy,' for NC: ', NC
       if (allocated(phi)) then
          continue
       else
          ALLOCATE(PHI(newN,7))
          ALLOCATE(BPHI(newN,7))
          write(*,*) 'Allocated: PHI, BPHI'
       endif
       if ((switch.eq.1).and.(N.ge.newN)) then
          write(*,*) 'Going from file WF'
c start with parameter in the WF.dat file
          do i=1,N/2
             read(9) PHI(i,1),PHI(i,2),PHI(i,3),PHI(i,4),PHI(i,5),PHI(i,6),PHI(i,7)
          enddo
          
          do i=newN/2+1,newN
             read(9) PHI(i,1),PHI(i,2),PHI(i,3),PHI(i,4),PHI(i,5),PHI(i,6),PHI(i,7)
          enddo

        else if (switch.eq.1) then
           write(*,*) 'Adding random parameters to file WF'
           do i=1,N/2
              read(9) PHI(i,1),PHI(i,2),PHI(i,3),PHI(i,4),PHI(i,5),PHI(i,6),PHI(i,7)
           enddo
           
           call RANDOM_SEED()
           do i=N/2+1,newN/2
              do j=1,7
                 call RANDOM_NUMBER(dev)
                 PHI(i,j)=dev*0.1
              enddo
           enddo
           do i=newN/2+1,newN/2+N/2        
              read(9) PHI(i,1),PHI(i,2),PHI(i,3),PHI(i,4),PHI(i,5),PHI(i,6),PHI(i,7)
           enddo
           call RANDOM_SEED()
           do i=newN/2+1+N/2,newN
              do j=1,7
                 call RANDOM_NUMBER(dev)
                 PHI(i,j)=dev*0.1
              enddo
           enddo
      else

c start with pseudo random variable
         write(*,*)'Using random parameters for WF'
         call RANDOM_SEED()
         do i=1,newN
            do j=1,7
               call RANDOM_NUMBER(dev)
               PHI(i,j)=dev*0.1
            enddo
         enddo
       endif
       if (allocated(HH)) then
          continue
       else
       ALLOCATE(HH(newN,newN))
       ALLOCATE(DN(newN,newN))
       ALLOCATE(HHOLD(newN,newN))
       ALLOCATE(QT(newN,newN))
       ALLOCATE(qc(newN))
       ALLOCATE(d(newN))
       WRITE(*,*) 'Allocated tables: HH,DN'
      endif
       
      CLOSE(9)

      END      
*********1*********2*********3*********4*********5*********6*********7** 

