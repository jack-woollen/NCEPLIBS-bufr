C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS SUBROUTINE UNPACKS AND RETURNS THE VALUES FOR ONE-
C>   DIMENSIONAL DESCRIPTORS IN THE INPUT STRING WITHOUT ADVANCING THE
C>   SUBSET POINTER.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"; IMPROVED MACHINE
C>                           PORTABILITY
C> 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-
C>                           LINING CODE WITH FPP DIRECTIVES
C> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           10,000 TO 20,000 BYTES
C> 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C>                           INCREASED FROM 15000 TO 16000 (WAS IN
C>                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C>                           WRF; ADDED DOCUMENTATION (INCLUDING
C>                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC
C>                           INFO WHEN ROUTINE TERMINATES ABNORMALLY
C> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> 2012-03-02  J. ATOR    -- USE FUNCTION UPS
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C> 2022-05-06  J. WOOLLEN -- REPLACE UPBB WITH UPB8 FOR 8BYTE INTEGERS
C>
C> USAGE:    CALL UFBGET (LUNIT, TAB, I1, IRET, STR)
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>     I1       - INTEGER: LENGTH OF TAB
C>     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B
C>                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH THE WORDS
C>                IN THE ARRAY TAB
C>                  - THERE ARE THREE "GENERIC" MNEMONICS NOT RELATED
C>                     TO TABLE B, THESE RETURN THE FOLLOWING
C>                     INFORMATION IN CORRESPONDING TAB LOCATION:
C>                     'NUL'  WHICH ALWAYS RETURNS BMISS ("MISSING")
C>                     'IREC' WHICH ALWAYS RETURNS THE CURRENT BUFR
C>                            MESSAGE (RECORD) NUMBER IN WHICH THIS
C>                            SUBSET RESIDES
C>                     'ISUB' WHICH ALWAYS RETURNS THE CURRENT SUBSET
C>                            NUMBER OF THIS SUBSET WITHIN THE BUFR
C>                            MESSAGE (RECORD) NUMBER 'IREC'
C>
C>   OUTPUT ARGUMENT LIST:
C>     TAB      - REAL*8: (I1) STARTING ADDRESS OF DATA VALUES READ FROM
C>                DATA SUBSET
C>     IRET     - INTEGER: RETURN CODE:
C>                       0 = normal return
C>                      -1 = there are no more subsets in the BUFR
C>                           message
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     INVWIN   STATUS   STRING
C>                               UPB8     UPC      UPS      USRTPL
C>    THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs.
C>
      SUBROUTINE UFBGET(LUNIT,TAB,I1,IRET,STR)

      USE MODV_BMISS
      USE MODA_USRINT
      USE MODA_USRBIT
      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)

      CHARACTER*(*) STR
      CHARACTER*8   CVAL
      EQUIVALENCE   (CVAL,RVAL)
      integer*8     ival,int8
      REAL*8        RVAL,TAB(I1),UPS

C-----------------------------------------------------------------------
      int(int8) = int8                    
C-----------------------------------------------------------------------

      IRET = 0

      DO I=1,I1
      TAB(I) = BMISS
      ENDDO

C  MAKE SURE A FILE/MESSAGE IS OPEN FOR INPUT
C  ------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE
C  ---------------------------------------------

      IF(NSUB(LUN).EQ.MSUB(LUN)) THEN
         IRET = -1
         GOTO 100
      ENDIF

C  PARSE THE STRING
C  ----------------

      CALL STRING(STR,LUN,I1,0)

C  EXPAND THE TEMPLATE FOR THIS SUBSET AS LITTLE AS POSSIBLE
C  ---------------------------------------------------------

      N = 1
      NBIT(N) = 0
      MBIT(N) = MBYT(LUN)*8 + 16
      CALL USRTPL(LUN,N,N)

10    DO N=N+1,NVAL(LUN)
      NODE = INV(N,LUN)
      NBIT(N) = IBT(NODE)
      MBIT(N) = MBIT(N-1)+NBIT(N-1)
      IF(NODE.EQ.NODS(NNOD)) THEN
         NVAL(LUN) = N
         GOTO 20
      ELSEIF(ITP(NODE).EQ.1) THEN
         CALL UPB8(IVAL,NBIT(N),MBIT(N),MBAY(1,LUN))
         CALL USRTPL(LUN,N,int(IVAL))
         GOTO 10
      ENDIF
      ENDDO
20    CONTINUE

C  UNPACK ONLY THE NODES FOUND IN THE STRING
C  -----------------------------------------

      DO I=1,NNOD
      NODE = NODS(I)
      INVN = INVWIN(NODE,LUN,1,NVAL(LUN))
      IF(INVN.GT.0) THEN
         CALL UPB8(IVAL,NBIT(INVN),MBIT(INVN),MBAY(1,LUN))
         IF(ITP(NODE).EQ.1) THEN
            TAB(I) = IVAL
         ELSEIF(ITP(NODE).EQ.2) THEN
            IF(IVAL.LT.2_8**(IBT(NODE))-1) TAB(I) = UPS(IVAL,NODE)
         ELSEIF(ITP(NODE).EQ.3) THEN
            CVAL = ' '
            KBIT = MBIT(INVN)
            CALL UPC(CVAL,NBIT(INVN)/8,MBAY(1,LUN),KBIT,.TRUE.)
            TAB(I) = RVAL
         ENDIF
      ELSE
         TAB(I) = BMISS
      ENDIF
      ENDDO

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBGET - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: UFBGET - INPUT BUFR FILE IS OPEN FOR OUTPUT'//
     . ', IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: UFBGET - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
      END
