      MODULE TABLES
c     N - size of basis
c     M - number of sectors
      integer N,M,newN,resume
c     BSTE - best energy, BPHI - best parameters PHI, BSTI - best index found
      real (8) BSTE
      integer BSTI
      real (8), DIMENSION(:,:), ALLOCATABLE :: BPHI

c     HH(N,N), DN(N,N) - hamiltonian and overlap matrix
      real (8), DIMENSION(:,:), ALLOCATABLE :: PHI,HH,DN,HHOLD,QT
      real (8), DIMENSION(:), ALLOCATABLE :: qc,d

c     updtbl - table wiwth updated basis elements, updtbl == 0 then full calculation, ==k single k=thelement to update
      integer updtbl
      
c     optional input-output parameters from init file
c     
c     EP - initial energy in inverse iteration,
c     foutWF=1 - write WF.dat output file with PHI table + expansion coefficients

      real (8)  EP,myacc,multEE,ESCALE
      integer foutWF,MAXIT,IPRINT,ICON
c     a,b,c value, a is not need, got eliminated on both side of the equation 
c     NC is the number of color 
      real(8) NC
      CHARACTER( LEN = 30 ) :: in_fileWF,out_fileWF

      END MODULE TABLES

************************************************************
      MODULE PRMTS
      INTEGER MAXF
      INTEGER, parameter :: NN = 6
      PARAMETER(MAXF=10000)
      real(8), parameter :: pi=3.1415926535897932384626433832795d0
      END MODULE PRMTS

*************************************************************
