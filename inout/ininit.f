      SUBROUTINE ininit(in_file)
      USE TABLES
      IMPLICIT NONE

      CHARACTER( LEN = 30 ) :: in_file

      CHARACTER C*11,str*54

      write(*,*) '*****************************************************'

      WRITE(*,*) 'Reading file : ', in_file
      OPEN(UNIT=9,FILE=in_file,FORM='FORMATTED')

      READ(9,10) C,EP
      write(*,*) C,EP

      READ(9,20) C,foutWF   
      write(*,*) C,foutWF   

      READ(9,*) str
      write(*,*) str

      READ(9,30) C,in_fileWF
      write(*,*) C,in_fileWF

      READ(9,30) C,out_fileWF
      write(*,*) C,out_fileWF

      READ(9,*) str
      write(*,*) str

      READ(9,10) C,myacc
      write(*,*) C,myacc

      READ(9,10) C,multEE
      write(*,*) C,multEE

      READ(9,10) C,ESCALE
      write(*,*) C,ESCALE

      READ(9,20) C,MAXIT
      write(*,*) C,MAXIT   

      READ(9,20) C,IPRINT
      write(*,*) C,IPRINT  
      
      READ(9,20) C,ICON
      write(*,*) C,ICON   

      READ(9,20) C,newN
      write(*,*) C,newN

      READ(9,20) C,resume
      write(*,*) C,resume

      READ(9,10) C,NC
      write(*,*) C,NC

      write(*,*) '****************************************************'

      CLOSE(9)

 100  continue

 10   FORMAT(A11,F54)
 20   FORMAT(A11,I10)
 30   FORMAT(A11,A30)

      END      
*********1*********2*********3*********4*********5*********6*********7** 

