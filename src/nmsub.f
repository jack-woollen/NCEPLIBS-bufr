C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS FUNCTION RETURNS THE NUMBER OF SUBSETS IN A BUFR
C>   MESSAGE OPEN FOR INPUT VIA A PREVIOUS CALL TO BUFR ARCHIVE LIBRARY
C>   SUBROUTINE READMG OR EQUIVALENT.  THE SUBSETS THEMSELVES DO NOT
C>   HAVE TO BE READ.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"
C> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C>                           TERMINATES ABNORMALLY
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    NMSUB (LUNIT)
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>
C>   OUTPUT ARGUMENT LIST:
C>     NMSUB    - INTEGER: NUMBER OF SUBSETS IN BUFR MESSAGE
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     STATUS
C>    THIS ROUTINE IS CALLED BY: UFBMNS   UFBPOS   UFBTAB   UFBTAM
C>                               Also called by application programs.
C>
      FUNCTION NMSUB(LUNIT)



      USE MODA_MSGCWD

      INCLUDE 'bufrlib.inc'

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NMSUB = 0

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      NMSUB = MSUB(LUN)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: NMSUB - INPUT BUFR FILE IS CLOSED, IT MUST '//
     . 'BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: NMSUB - INPUT BUFR FILE IS OPEN FOR OUTPUT,'//
     . ' IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: NMSUB - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
      END
