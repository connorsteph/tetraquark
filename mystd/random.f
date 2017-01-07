
*********1*********2*********3*********4*********5*********6*********7** 

      subroutine RMARIN(IJ,KL)
C This is the initialization routine for the random number generator RANMAR()
C NOTE: The seed variables can have values between:    0 <= IJ <= 31328
C                                                      0 <= KL <= 30081
C The random number sequences created by these two seeds are of sufficient 
C length to complete an entire calculation with. For example, if sveral 
C different groups are working on different parts of the same calculation,
C each group could be assigned its own IJ seed. This would leave each group
C with 30000 choices for the second seed. That is to say, this random 
C number generator can create 900 million different subsequences -- with 
C each subsequence having a length of approximately 10^30.
C 
C Use IJ = 1802 & KL = 9373 to test the random number generator. The
C subroutine RANMAR should be used to generate 20000 random numbers.
C Then display the next six random numbers generated multiplied by 4096*4096
C If the random number generator is working properly, the random numbers
C should be:
C           6533892.0  14220222.0  7275067.0
C           6172232.0  8354498.0   10633180.0


      real U(97), C, CD, CM
      integer I97, J97
      logical TEST,tesst
      data TEST /.FALSE./
      common /raset1/ U, C, CD, CM, I97, J97, TESst
      tesst=test    
      if( IJ .lt. 0  .or.  IJ .gt. 31328  .or.
     *    KL .lt. 0  .or.  KL .gt. 30081 ) then
          print '(A)', ' The first random number seed must have a value 
     *between 0 and 31328'
          print '(A)',' The second seed must have a value between 0 and  
     *30081'
            stop
      endif

      i = mod(IJ/177, 177) + 2
      j = mod(IJ    , 177) + 2
      k = mod(KL/169, 178) + 1
      l = mod(KL,     169) 

      do 2 ii = 1, 97
         s = 0.0
         t = 0.5
         do 3 jj = 1, 24
            m = mod(mod(i*j, 179)*k, 179)
            i = j
            j = k
            k = m
            l = mod(53*l+1, 169)
            if (mod(l*m, 64) .ge. 32) then
               s = s + t
            endif
            t = 0.5 * t
3        continue
         U(ii) = s
2     continue

      C = 362436.0 / 16777216.0
      CD = 7654321.0 / 16777216.0
      CM = 16777213.0 /16777216.0

      I97 = 97
      J97 = 33

      TESsT = .TRUE.
      return
      end

      subroutine ranmar(RVEC, LEN)
C This is the random number generator proposed by George Marsaglia in 
C Florida State University Report: FSU-SCRI-87-50
C It was slightly modified by F. James to produce an array of pseudorandom
C numbers.
      REAL RVEC(*)
      real U(97), C, CD, CM
      integer I97, J97
      logical TEST
      common /raset1/ U, C, CD, CM, I97, J97, TEST
 
      integer ivec
 
      if( .NOT. TEST ) then
         print '(A)',' Call the init routine (RMARIN) before calling RAN        
     *MAR'  
         stop
      endif

      do 100 ivec = 1, LEN
         uni = U(I97) - U(J97)
         if( uni .lt. 0.0 ) uni = uni + 1.0
         U(I97) = uni
         I97 = I97 - 1
         if(I97 .eq. 0) I97 = 97
         J97 = J97 - 1
         if(J97 .eq. 0) J97 = 97
         C = C - CD
         if( C .lt. 0.0 ) C = C + CM
         uni = uni - C
         if( uni .lt. 0.0 ) uni = uni + 1.0
         RVEC(ivec) = uni
100   continue
      return
      end

*********1*********2*********3*********4*********5*********6*********7** 
