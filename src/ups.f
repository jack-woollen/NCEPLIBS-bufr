C> @file
C> @author J @date 2012-03-02
	
C> THIS FUNCTION UNPACKS A REAL*8 USER VALUE FROM A PACKED
C>   BUFR INTEGER BY APPLYING THE PROPER SCALE AND REFERENCE VALUES.
C>   NORMALLY THE SCALE AND REFERENCE VALUES ARE OBTAINED FROM INDEX
C>   NODE OF THE INTERNAL JUMP/LINK TABLE ARRAYS ISC(*) AND IRF(*);
C>   HOWEVER, THE REFERENCE VALUE IN IRF(*) WILL BE OVERRIDDEN IF A
C>   2-03 OPERATOR IS IN EFFECT FOR THIS NODE.
C>
C> PROGRAM HISTORY LOG:
C> 2012-03-02  J. ATOR    -- ORIGINAL AUTHOR; ADAPTED FROM INTERNAL
C>                           STATEMENT FUNCTION IN OTHER SUBROUTINES
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C> 2022-05-06  J. WOOLLEN -- MAKE IVAL AND IMASK 8BYTE INTEGERS  
C>
C> USAGE:    UPS (IVAL,NODE)
C>   INPUT ARGUMENT LIST:
C>     IVAL      - INTEGER: PACKED BUFR INTEGER
C>     NODE      - INTEGER: INDEX INTO INTERNAL JUMP/LINK TABLES
C>
C>   OUTPUT ARGUMENT LIST:
C>     UPS       - REAL*8: USER VALUE
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        None
C>    THIS ROUTINE IS CALLED BY: RDCMPS   RDTREE   UFBGET   UFBTAB
C>                               UFBTAM
C>                               Normally not called by any application
C>                               programs.
C>
	REAL*8 FUNCTION UPS(IVAL,NODE)

	USE MODA_TABLES
	USE MODA_NRV203

        integer*8 ival,imask
	REAL*8	  TEN

	DATA TEN /10./

C-----------------------------------------------------------------------

	UPS = ( IVAL + IRF(NODE) ) * TEN**(-ISC(NODE))

	IF ( NNRV .GT. 0 ) THEN

C	  There are redefined reference values in the jump/link table,
C	  so we need to check if this node is affected by any of them.

	  DO JJ = 1, NNRV
	    IF ( NODE .EQ. INODNRV(JJ) ) THEN

C	      This node contains a redefined reference value.
C	      Per the rules of BUFR, negative values may be encoded
C	      as positive integers with the left-most bit set to 1.

	      IMASK = 2_8**(IBT(NODE)-1)
	      IF ( IAND(IVAL,IMASK) .GT. 0 ) THEN
		NRV(JJ) = (-1) * ( IVAL - IMASK )
	      ELSE
		NRV(JJ) = IVAL
	      END IF
	      UPS = NRV(JJ)
	      RETURN
	    ELSE IF ( ( TAG(NODE)(1:8) .EQ. TAGNRV(JJ) ) .AND.	    
     .		      ( NODE .GE. ISNRV(JJ) ) .AND.
     .		      ( NODE .LE. IENRV(JJ) ) ) THEN

C	      The corresponding redefinded reference value needs to
C	      be used when decoding this value.

	      UPS = ( IVAL + NRV(JJ) ) * TEN**(-ISC(NODE))
	      RETURN
	    END IF
	  END DO

	END IF

	RETURN
	END
