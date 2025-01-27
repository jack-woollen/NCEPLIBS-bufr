C> @file
C> @brief Remove leading and trailing blanks from a character string

C> This subroutine removes leading and trailing blanks from a
C> character string.  The string may not contain any embedded blanks.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in]  STR1 -- character*(*): String
C> @param[out] STR2 -- character*(*): Copy of STR1 with leading and
C>                     trailing blanks removed
C> @param[out] LENS -- integer: Length of STR2
C>                     - -1 = STR1 contained embedded blanks
C>
C> <b>Program History Log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author
C> | 2003-11-04 | J. Ator    | Added documentation
C> | 2009-04-21 | J. Ator    | Use errwrt()
C>
      SUBROUTINE STRSUC(STR1,STR2,LENS)

      CHARACTER*(*) STR1,STR2

      COMMON /QUIET / IPRT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      LENS = 0
      LSTR = LEN(STR1)

C  FIND THE FIRST NON-BLANK IN THE INPUT STRING
C  --------------------------------------------

      DO I=1,LSTR
      IF(STR1(I:I).NE.' ') GOTO 2
      ENDDO
      GOTO 100

C     Now, starting with the first non-blank in the input string,
C     copy characters from the input string into the output string
C     until reaching the next blank in the input string.

2     DO J=I,LSTR
      IF(STR1(J:J).EQ.' ') GOTO 3
      LENS = LENS+1
      STR2(LENS:LENS) = STR1(J:J)
      ENDDO
      GOTO 100

C     Now, continuing on within the input string, make sure that
C     there are no more non-blank characters.  If there are, then
C     the blank at which we stopped copying from the input string
C     into the output string was an embedded blank.

3     DO I=J,LSTR
      IF(STR1(I:I).NE.' ') LENS = -1
      ENDDO

      IF(LENS.EQ.-1 .AND. IPRT.GE.0)  THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: STRSUC - INPUT STRING:')
      CALL ERRWRT(STR1)
      CALL ERRWRT('CONTAINS ONE OR MORE EMBEDDED BLANKS')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
