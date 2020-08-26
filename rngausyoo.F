***********************************************************************
*     ----------------------------------
      SUBROUTINE RNGAUSYOO(RND)
*     ----------------------------------
*     
*     ( purpose )
*       generate gaussian distributed randum number
*       For Water Scattering laser beam width
*
*     ( input ) 
*       XMEAN : MEAN OF DISTRIBUTION
*       SIGMA : SIGMA OF DISTRIBUTION
*
*     ( output ) 
*       RND   : RANDUM ANGLE 
*
*     ( creation date and author )
*       ????.??.?? First version by J.H. Yoo?
*
***********************************************************************


      real rdummy,randNum,rnd, mini, ang, collVal, SIGMA, min, minloc
      integer I, miniPos
      real::cdfVec(301), vectorMin(301), collProf(301), cdfVecNorm(301)
      real::angVec(301)
      

C     Read the collimator profile data into vectors and calculate CDF
      open(unit=10,file='/disk02/usr6/pmehta/detsim_200220_koshio_modified_coll_prof/B1coll_prof.txt')
c      write (*,*) 'opened file'
      do  i = 1, 301
c         write (*,*) 'hello'
         read(10,*) ang, collVal
         collProf(i) = collVal
         angVec(i) = ang
         if (i==1) then
            cdfVec(i) = collProf(i)
         else
            cdfVec(i) = collProf(i) + cdfVec(i-1)
         end if
c         print *, ang, collVal, cdfVec(i)
      end do 
      close(10)


C     Work out the minimum and maximum value
      cdfVecMin = MINVAL(cdfVec)
      cdfVecMax = MAXVAL(cdfVec)
c      write (*,*)'cdfvecmin,cdfvecmax=', cdfVecMin, cdfVecMax
      
C     Do Min-Max normalisation to get CDF values between 0 and 1
      do i=1,301
         cdfVecNorm(i) = (cdfVec(i)-cdfVecMin)/(cdfVecMax-cdfVecMin)
c         print *, ang, collVal, cdfVec(i), cdfVecNorm(i)
c         write (*,*)'cdfVecNorm =', cdfVecNorm
      end do

C     Find the minimum value of the data, generate random number between 0 and 1, take this away from normalised CDF values and find the minimum
      Call GRNDM(RDUMMY,1)
c      print *, RDUMMY
c      RDUMMY = 0.52763
c      print *, RDUMMY
      do  i=1, 301
         vectorMin(i) = ABS(cdfVecNorm(i) - RDUMMY)
c         print *, cdfVecNorm(i), vectorMin(i)
c         write (*,*)'vectorMin=', vectorMin
      end do

      RND  = angVec(MINLOC(vectorMin,1))
c      write(*,*)'value of RND =',RND

c     170820 RND ang thing

      ang = sqrt(RND**2/(3610.**2))
      ang = sqrt(ang)
      RND = atan(ang)
       
c      write(*,*)'ang  ;',ang
c      write(*,*)'ANGLE:',RND

      RETURN
      END      
