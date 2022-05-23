!-----------------------------------------------------------------------
!  MAIN PROGRAM CMPT29
!-----------------------------------------------------------------------
      PROGRAM CMPT29

      CHARACTER*80 FILe,tbldir
      CHARACTER*60 STR 
      CHARACTER*50 HEADR,OBSTR,QMSTR,FCSTR,ERSTR,QMSFC
      CHARACTER*20 VARS(7)
      CHARACTER*8  SUBSET,DATE
      DIMENSION    KNT(1024,7,0:17),HDR(5),OBS(8,255),QMS(8,255)
      LOGICAL      exist
      REAL*8       HDR,OBS,QMS

      DATA HEADR /'SID XOB YOB DHR T29              '/
      DATA OBSTR /'POB QOB TOB ZOB UOB PWO RHO VOB  '/
      DATA QMSTR /'PQM QQM TQM ZQM WQM PWQ RHQ      '/

      DATA VARS   /'PRESSURE        ',&
                   'SPECIFIC HUMIDTY',&
                   'TEMPERATURE     ',&
                   'HEIGHT          ',&
                   'WIND COMPONENTS ',&
                   'PRECIPITABLE H2O',&
                   'RELATIVE HUMIDTY'/

      DATA LUBFR /8    /
      DATA VMAX  /10E10/

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IREC = 0
      KNT = 0

!  get filename argument

      NARG=IARGC()
      IF(NARG<1) THEN
        write(*,*)'Usage: "t29inv <prepbufr>" will print prepbufr inventory by variable and ON29 report type'
        CALL EXIT(2)
      ENDIF
      call getarg(1,file)
      file = TRIM(adjustl(file))
      inquire(file=file,exist=exist)
      if (.not.exist) call bort(trim(file)//' does not exist') 

      open(lubfr,file=file,form='unformatted')
      CALL OPENBF(LUBFR,'IN',LUBFR)
      CALL READMG(LUBFR,SUBSET,IDATE,IRET)
      IF(IRET.NE.0) GOTO 900
      WRITE(DATE,'(I8)') IDATE
      DO I=1,8
      IF(DATE(I:I).EQ.' ') DATE(I:I) = '0'
      ENDDO
      PRINT'(''DATA  VALID AT  '',A8)',DATE

!  define master table directory

      tbldir='/scratch1/NCEPDEV/global/Jack.Woollen/jwfork/build/tables'
      call mtinfo(tbldir,3,4)
      call codflg('Y')

!  READ THRU THE PREPDA RECORDS
!  ----------------------------

10    CALL READSB(LUBFR,IRET)
      IF(IRET.NE.0) THEN
         CALL READMG(LUBFR,SUBSET,IDATE,IRET)
         IF(IRET.NE.0) GOTO 100
         CALL UFBCNT(LUBFR,IREC,ISUB)
         GOTO 10
      ENDIF
      QMS = 10E10
      CALL UFBINT(LUBFR,HDR,5,1,IRET,HEADR)
      CALL UFBINT(LUBFR,OBS,8,255,NLEV,OBSTR)
      CALL UFBINT(LUBFR,QMS,8,255,NLEV,QMSTR)

      KX = HDR(5)
      if(kx.lt.0.or.kx.gt.1024) print*,'missing ',kx
      if(kx.lt.0.or.kx.gt.1024) goto 10

      DO L=1,NLEV
      DO K=1,7
      IQ = -1
      IF(K.EQ.5) OBS(5,L) = MAX(OBS(5,L),OBS(8,L))
      IF(OBS(K,L).LT.VMAX .AND. QMS(K,L).LT.VMAX) THEN
         IQ = QMS(K,L)
      ELSEIF(OBS(K,L).LT.VMAX .AND. QMS(K,L).GE.VMAX) THEN
         IQ = 16
      ELSEIF(OBS(K,L).GE.VMAX .AND. QMS(K,L).LT.VMAX) THEN
         IQ = 17
      ENDIF
      !print*,iq,kx,k
      IF(IQ.GE.0) KNT(KX,K,IQ) = KNT(KX,K,IQ)+1
      ENDDO
      ENDDO

      GOTO 10

! need to open the bufrfile with the satellites of interest

100   call closbf(lubfr)
      open(lubfr,file=file,form='unformatted')
      CALL OPENBF(LUbfr,'IN',LUbfr)
      call readmg(lubfr,subset,idate,iret)
      call codflg('Y')

!  FINISH UP
!  ---------

      DO K=1,7
      PRINT*,VARS(K)
      PRINT*
      DO KX=1,1024
      ITOT = 0
      DO IQ=0,17
      ITOT = ITOT+KNT(KX,K,IQ)
      ENDDO
      IF(ITOT.GT.0) str=repeat(' ',len(str))
      IF(ITOT.GT.0) call getcfmng(lubfr,'T29',kx,' ',-1,str,lenstr,iret)
      IF(ITOT.GT.0) PRINT 101,KX,ITOT,trim(str)  
101   FORMAT(I3,I8,3x,a)       
      ENDDO
      PRINT*
      ENDDO

      PRINT*,'******CMPBQM PROCESSED ',IREC,' BUFR RECORDS******'
      STOP
900   CALL BORT('CMPBQM - ERROR READING BUFR FILE ')
      END

