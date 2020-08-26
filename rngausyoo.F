***********************************************************************
*     ----------------------------------
      SUBROUTINE RNGAUSYOO(XMEAN,SIGMA,RND)
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


      real a,rdummy,sigma,xmean,rnd,ang
      integer I

      A=0.0

      DO 10 I=1,12
         Call GRNDM(RDUMMY,1)
         A=A+RDUMMY
 10   CONTINUE
!Writing output file for variables - PM
c      open(unit=40,file='output.dat')
c      write(*,*)'A    : ',A      
      print *, 'A:' ,A
      RND = (A-6.)*SIGMA+XMEAN
      print *,'RND:', RND
c      write(*,*)'RND    : ',RND
      ang = sqrt(RND**2/(3610.**2))
      ang = sqrt(ang)
      RND = atan(ang)
!Writing output for variables - PM
c      write(*,*)'ang:',ang
c      write(*,*)'ANGLE:',RND
c      write(*,*)'SIGMA:',SIGMA
c      write(*,*)'XMEAN:',XMEAN            
       print *, 'ang:', ang
       print *, 'ANGLE;',RND
       print *, 'SIGMA:',SIGMA
       print *, 'XMEAN:',XMEAN 
c      close(40)
      RETURN
      END

