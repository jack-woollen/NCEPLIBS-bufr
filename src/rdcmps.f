C> @file
C> @author WOOLLEN @date 2000-09-19
      
C> THIS SUBROUTINE UNCOMPRESSES AND UNPACKS THE NEXT SUBSET
C>   FROM THE INTERNAL COMPRESSED MESSAGE BUFFER (ARRAY MBAY IN MODULE
C>   BITBUF) AND STORES THE UNPACKED SUBSET WITHIN THE INTERNAL
C>   ARRAY VAL(*,LUN) IN MODULE USRINT.
C>
C> PROGRAM HISTORY LOG:
C> 2000-09-19  J. WOOLLEN -- ORIGINAL AUTHOR
C> 2002-05-14  J. WOOLLEN -- IMPROVED GENERALITY, PREVIOUSLY RDCMPS
C>                           WOULD NOT RECOGNIZE COMPRESSED DELAYED
C>                           REPLICATION AS A LEGITIMATE DATA STRUCTURE
C> 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C>                           INCREASED FROM 15000 TO 16000 (WAS IN
C>                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C>                           WRF; ADDED HISTORY DOCUMENTATION
C> 2004-08-18  J. ATOR    -- INITIALIZE CVAL TO EMPTY BEFORE CALLING UPC;
C>                           CORRECT LOGIC FOR WHEN A CHARACTER VALUE IS
C>                           THE SAME FOR ALL SUBSETS IN A MESSAGE;
C>                           MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> 2009-03-23  J. ATOR    -- PREVENT OVERFLOW OF CVAL AND CREF FOR
C>                           STRINGS LONGER THAN 8 CHARACTERS
C> 2012-03-02  J. ATOR    -- USE FUNCTION UPS
C> 2012-06-04  J. ATOR    -- SET DECODED REAL*8 VALUE TO "MISSING" WHEN
C>                           CORRESPONDING CHARACTER FIELD HAS ALL BITS
C>                           SET TO 1
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C> 2022-05-06  J. WOOLLEN -- USE UP8 FOR 8BYTE INTEGER OPERATION 
C>
C> USAGE:    CALL RDCMPS (LUN)
C>   INPUT ARGUMENT LIST:
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     ICBFMS   IGETRFEL STRBTM
C>                               UPB      UP8      UPC      UPS      USRTPL
C>    THIS ROUTINE IS CALLED BY: READSB
C>                               Normally not called by any application
C>                               programs.
C>
      SUBROUTINE RDCMPS(LUN)

      USE MODV_BMISS

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES
      USE MODA_RLCCMN

      CHARACTER*128 BORT_STR
      CHARACTER*8   CREF,CVAL
      EQUIVALENCE   (CVAL,RVAL)
      REAL*8        RVAL,UPS

      integer(8) :: ival,lref,ninc,lps

C-----------------------------------------------------------------------
C     Statement function to compute BUFR "missing value" for field
C     of length LBIT bits (all bits "on"):

      LPS(LBIT) = MAX(2_8**(LBIT)-1,1)
C-----------------------------------------------------------------------

C  SETUP THE SUBSET TEMPLATE
C  -------------------------

      CALL USRTPL(LUN,1,1)

C  UNCOMPRESS A SUBSET INTO THE VAL ARRAY ACCORDING TO TABLE B
C  -----------------------------------------------------------

      NSBS = NSUB(LUN)

C     Note that we are going to unpack the (NSBS)th subset from within
C     the current BUFR message.

      IBIT = MBYT(LUN)
      NRST = 0

C     Loop through each element of the subset.

      N = 0

1     DO N=N+1,NVAL(LUN)
      NODE = INV(N,LUN)
      NRFELM(N,LUN) = IGETRFEL(N,LUN)
      NBIT = IBT(NODE)
      ITYP = ITP(NODE)

C     In each of the following code blocks, the "local reference value"
C     for the element is determined first, followed by the 6-bit value
C     which indicates how many bits are used to store the increment
C     (i.e. offset) from this "local reference value".  Then, we jump
C     ahead to where this increment is stored for this particular subset,
C     unpack it, and add it to the "local reference value" to determine
C     the final uncompressed value for this element from this subset.

C     Note that, if an element has the same final uncompressed value
C     for each subset in the message, then the encoding rules for BUFR
C     compression dictate that the "local reference value" will be equal
C     to this value, the 6-bit increment length indicator will have
C     a value of zero, and the actual increments themselves will be
C     omitted from the message.

      IF(ITYP.EQ.1.OR.ITYP.EQ.2) THEN

C        This is a numeric element.

         CALL UP8(LREF,NBIT,MBAY(1,LUN),IBIT)
         CALL UPB(LINC,   6,MBAY(1,LUN),IBIT)
         JBIT = IBIT + LINC*(NSBS-1)
         CALL UP8(NINC,LINC,MBAY(1,LUN),JBIT)
         IF(NINC.EQ.LPS(LINC)) THEN
            IVAL = LPS(NBIT)
         ELSE
            IVAL = LREF+NINC
         ENDIF
         IF(ITYP.EQ.1) THEN
            NBMP=IVAL; CALL USRTPL(LUN,N,NBMP)
            GOTO 1
         ENDIF
         IF(IVAL.LT.LPS(NBIT)) VAL(N,LUN) = UPS(IVAL,NODE)
         CALL STRBTM(N,LUN)
         IBIT = IBIT + LINC*MSUB(LUN)
      ELSEIF(ITYP.EQ.3) THEN

C        This is a character element.  If there are more than 8
C        characters, then only the first 8 will be unpacked by this
C        routine, and a separate subsequent call to BUFR archive library
C        subroutine READLC will be required to unpack the remainder of
C        the string.  In this case, pointers will be saved within
C        COMMON /RLCCMN/ for later use within READLC.

C        Unpack the local reference value.

         LELM = NBIT/8
         NCHR = MIN(8,LELM)
         IBSV = IBIT
         CREF = ' '
         CALL UPC(CREF,NCHR,MBAY(1,LUN),IBIT,.TRUE.)
         IF(LELM.GT.8) THEN
            IBIT = IBIT + (LELM-8)*8
            NRST = NRST + 1
            IF(NRST.GT.MXRST) GOTO 900
            CRTAG(NRST) = TAG(NODE)
         ENDIF

C        Unpack the increment length indicator.  For character elements,
C        this length is in bytes rather than bits.

         CALL UPB(LINC,   6,MBAY(1,LUN),IBIT)
         IF(LINC.EQ.0) THEN
            IF(LELM.GT.8) THEN
               IRNCH(NRST) = LELM
               IRBIT(NRST) = IBSV
            ENDIF
            CVAL = CREF
         ELSE
            JBIT = IBIT + LINC*(NSBS-1)*8
            IF(LELM.GT.8) THEN
               IRNCH(NRST) = LINC
               IRBIT(NRST) = JBIT
            ENDIF
            NCHR = MIN(8,LINC)
            CVAL = ' '
            CALL UPC(CVAL,NCHR,MBAY(1,LUN),JBIT,.TRUE.)
         ENDIF
         IF (LELM.LE.8 .AND. ICBFMS(CVAL,NCHR).NE.0) THEN
            VAL(N,LUN) = BMISS
         ELSE
            VAL(N,LUN) = RVAL
         ENDIF
         IBIT = IBIT + 8*LINC*MSUB(LUN)
      ENDIF
      ENDDO

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: RDCMPS - NUMBER OF LONG CHARACTER ' //
     .   'STRINGS EXCEEDS THE LIMIT (",I4,")")') MXRST
      CALL BORT(BORT_STR)
      END
