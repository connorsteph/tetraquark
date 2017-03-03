      SUBROUTINE outWF(VPHI,V,out_file)
      USE TABLES
      IMPLICIT NONE

      real (8) VPHI(N,6),V(N)
      CHARACTER( LEN = 30 ) :: out_file
      integer i

      WRITE(*,*) 'Writing file : ', out_file
      OPEN(UNIT=9,FILE=out_file,FORM='UNFORMATTED')

      write(9) N
      write(9) BSTE
      write(9) NC
      do i=1,N
         write(9) VPHI(i,1),VPHI(i,2),VPHI(i,3),VPHI(i,4),VPHI(i,5),VPHI(i,6),V(i)
      enddo

      CLOSE(9)

      END      
*********1*********2*********3*********4*********5*********6*********7** 
