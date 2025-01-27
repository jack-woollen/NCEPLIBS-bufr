C> @file
C> @brief Get information about a Table B descriptor

C> This subroutine returns information about a Table B descriptor
C> from the internal DX BUFR tables.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUN -- integer: Internal I/O stream index associated
C>                   with DX BUFR tables
C> @param[in] ITAB -- integer: Positional index of descriptor within
C>                    internal Table B
C> @param[out] UNIT -- character*24: Units of descriptor
C> @param[out] ISCL -- integer: Scale factor of descriptor
C> @param[out] IREF -- integer: Reference value of descriptor
C> @param[out] IBIT -- integer: Bit width of descriptor
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1995-06-28 | J. Woollen | Increased the size of internal BUFR table arrays in order to handle bigger files |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | Changed call to function "val$" to valx() |
C> | 2003-11-04 | J. Ator    | Added documentation |
C> | 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C>
      SUBROUTINE NEMTBB(LUN,ITAB,UNIT,ISCL,IREF,IBIT)

      USE MODA_TABABD

      CHARACTER*128 BORT_STR
      CHARACTER*24  UNIT
      CHARACTER*8   NEMO
      REAL*8        MXR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      MXR = 1E11-1

      IF(ITAB.LE.0 .OR. ITAB.GT.NTBB(LUN)) GOTO 900

C  PULL OUT TABLE B INFORMATION
C  ----------------------------

      IDN  = IDNB(ITAB,LUN)
      NEMO = TABB(ITAB,LUN)( 7:14)
      UNIT = TABB(ITAB,LUN)(71:94)
      ISCL = VALX(TABB(ITAB,LUN)( 95: 98))
      IREF = VALX(TABB(ITAB,LUN)( 99:109))
      IBIT = VALX(TABB(ITAB,LUN)(110:112))

C  CHECK TABLE B CONTENTS
C  ----------------------

      IF(IDN.LT.IFXY('000000')) GOTO 901
      IF(IDN.GT.IFXY('063255')) GOTO 901

      IF(ISCL.LT.-999 .OR. ISCL.GT.999) GOTO 902
      IF(IREF.LE.-MXR .OR. IREF.GE.MXR) GOTO 903
      IF(IBIT.LE.0) GOTO 904
      IF(UNIT(1:5).NE.'CCITT' .AND. IBIT.GT.32      ) GOTO 904
      IF(UNIT(1:5).EQ.'CCITT' .AND. MOD(IBIT,8).NE.0) GOTO 905

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - ITAB (",I7,") NOT FOUND IN '//
     . 'TABLE B")') ITAB
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - INTEGER REPRESENTATION OF '//
     . 'DESCRIPTOR FOR TABLE B MNEMONIC ",A," (",I7,") IS OUTSIDE '//
     . 'RANGE 0-16383 (16383 -> 0-63-255)")') NEMO,IDN
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - SCALE VALUE FOR TABLE B '//
     .'MNEMONIC ",A," (",I7,") IS OUTSIDE RANGE -999 TO 999")')
     . NEMO,ISCL
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - REFERENCE VALUE FOR TABLE B'//
     .' MNEMONIC ",A," (",I7,") IS OUTSIDE RANGE +/- 1E11-1")')
     . NEMO,IREF
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - BIT WIDTH FOR NON-CHARACTER'//
     . ' TABLE B MNEMONIC ",A," (",I7,") IS > 32")') NEMO,IBIT
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - BIT WIDTH FOR CHARACTER '//
     . 'TABLE B MNEMONIC ",A," (",I7,") IS NOT A MULTIPLE OF 8")')
     . NEMO,IBIT
      CALL BORT(BORT_STR)
      END
