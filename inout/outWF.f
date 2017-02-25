       SUBROUTINE outWF(VPHI,V,out_file)
       USE TABLES
       USE PRMTS
       IMPLICIT NONE

       real (8) VPHI(N,NPAR),V(N)
       CHARACTER( LEN = 30 ) :: out_file
       integer i,j

       WRITE(*,*) 'Writing file : ', out_file
       OPEN(UNIT=9,FILE=out_file,FORM='UNFORMATTED')

       write(9) N
       write(9) BSTE
       write(9) NC
       do i=1,N
        write(9) (VPHI(i,j),j=1,NPAR),V(i)
       enddo

       CLOSE(9)

      END      
*********1*********2*********3*********4*********5*********6*********7** 
