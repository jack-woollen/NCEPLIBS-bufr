C> @file
C> @author ATOR @date 2007-01-19
	
C> THIS FUNCTION LOOKS FOR AND RETURNS A VALID FXY NUMBER
C>   FROM WITHIN THE GIVEN INPUT STRING.  THE FXY NUMBER MAY BE IN
C>   FORMAT OF EITHER FXXYYY OR F-XX-YYY WITHIN THE INPUT STRING, BUT
C>   IT IS ALWAYS RETURNED IN FORMAT FXXYYY UPON OUTPUT.
C>
C> PROGRAM HISTORY LOG:
C> 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR
C> - 2021-09-30  J. Ator    -- Replace jstchr with Fortran intrinsic
C>                             adjustl
C>
C> USAGE:    IGETFXY ( STR, CFXY )
C>   INPUT ARGUMENT LIST:
C>     STR      - CHARACTER*(*): INPUT STRING
C>
C>   OUTPUT ARGUMENT LIST:
C>     CFXY     - CHARACTER*6: FXY NUMBER IN FORMAT FXXYYY
C>     IGETFXY  - INTEGER: RETURN CODE:
C>                       0 = normal return
C>                      -1 = could not find a valid FXY number in STR
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        NUMBCK
C>    THIS ROUTINE IS CALLED BY: GETNTBE  SNTBDE   SNTBFE
C>                               Normally not called by any application
C>                               programs.
C>
	FUNCTION IGETFXY ( STR, CFXY )



	CHARACTER*(*)	STR
	CHARACTER*6	CFXY

	PARAMETER  ( LSTR2 = 120 )
	CHARACTER*(LSTR2)  STR2

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IGETFXY = -1

	LSTR = LEN ( STR )
	IF ( LSTR .LT. 6 ) RETURN

C	Left-justify a copy of the input string.

	IF ( LSTR .GT. LSTR2 ) THEN
	    STR2(1:LSTR2) = STR(1:LSTR2)
	ELSE
	    STR2 = STR
	ENDIF
	STR2 = ADJUSTL ( STR2 )
	IF ( STR2 .EQ. ' ' ) RETURN

C	Look for an FXY number.

	IF ( INDEX ( STR2, '-' ) .NE. 0 ) THEN
C	    Format of field is F-XX-YYY.
	    CFXY(1:1) = STR2(1:1)
	    CFXY(2:3) = STR2(3:4)
	    CFXY(4:6) = STR2(6:8)
	ELSE
C	    Format of field is FXXYYY.
	    CFXY = STR2(1:6)
	ENDIF

C	Check that the FXY number is valid.

	IF ( NUMBCK ( CFXY ) .EQ. 0 ) IGETFXY = 0

	RETURN
	END
