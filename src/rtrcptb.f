C> @file
C> @brief Read the tank receipt time from Section 1 of a BUFR message.

C> This subroutine reads the tank receipt time (if one exists) from
C> Section 1 of a BUFR message.  It is similar to subroutine rtrcpt(),
C> except that it operates on a BUFR message passed in via a memory
C> array, whereas rtrcpt() operates on the BUFR message that was read
C> into internal arrays via the most recent call to any of the other
C> [message-reading subroutines](@ref hierarchy) for a specified
C> Fortran logical unit.
C>
C> @author J. Ator
C> @date 2013-10-07
C>
C> @param[in]  MBAY -- integer(*): BUFR message
C> @param[out] IYR  -- integer: Tank receipt year
C> @param[out] IMO  -- integer: Tank receipt month
C> @param[out] IDY  -- integer: Tank receipt day
C> @param[out] IHR  -- integer: Tank receipt hour
C> @param[out] IMI  -- integer: Tank receipt minute
C> @param[out] IRET -- integer: return code
C>                     - 0 = normal return
C>                     - -1 = no tank receipt time exists within
C>                            MBAY
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2013-10-07 | J. Ator | Original author; adapted from rtrcpt() |
C>
      SUBROUTINE RTRCPTB(MBAY,IYR,IMO,IDY,IHR,IMI,IRET)

      DIMENSION	MBAY (*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = -1

C     Check whether the message contains a tank receipt time.

      IF(IUPBS01(MBAY,'BEN').EQ.4) THEN
	IS1BYT = 23
      ELSE
	IS1BYT = 19
      ENDIF
      IF( (IS1BYT+5) .GT. IUPBS01(MBAY,'LEN1') ) RETURN

C     Unpack the tank receipt time.

C     Note that IS1BYT is a starting byte number relative to the
C     beginning of Section 1, so we still need to account for
C     Section 0 when specifying the actual byte numbers to unpack
C     within the overall message.

      IMGBYT = IS1BYT + IUPBS01(MBAY,'LEN0')

      IYR = IUPB(MBAY,IMGBYT,16)
      IMO = IUPB(MBAY,IMGBYT+2,8)
      IDY = IUPB(MBAY,IMGBYT+3,8)
      IHR = IUPB(MBAY,IMGBYT+4,8)
      IMI = IUPB(MBAY,IMGBYT+5,8)

      IRET = 0

      RETURN
      END
