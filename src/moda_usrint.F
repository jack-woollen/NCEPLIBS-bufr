	MODULE MODA_USRINT

#ifndef MAXSS_H
#define MAXSS_H
	  USE MODV_MAXSS
#endif

#ifndef NFILES_H
#define NFILES_H
	  USE MODV_NFILES
#endif

	  INTEGER, ALLOCATABLE :: NVAL(:)
	  INTEGER, TARGET, ALLOCATABLE :: INV(:,:)
	  INTEGER, ALLOCATABLE :: NRFELM(:,:)
	  REAL*8, TARGET, ALLOCATABLE :: VAL(:,:)

	END MODULE
