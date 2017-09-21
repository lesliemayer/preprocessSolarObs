MODULE gong_parameters

IMPLICIT NONE
SAVE

!---------------------
! Size of data arrays
!---------------------
INTEGER :: iDim=-9999, jDim=-9999

!--------------------------------
! grid resolution of input data
!--------------------------------
! Model Parameter settings

CHARACTER(4) :: obsName='gong'

!----------------------------------------------------
! leading carrington rotation, longitude and version
!----------------------------------------------------
INTEGER :: carrRotation=-9999, carrVersion=-9999
REAL :: carrLong=-9999.

REAL :: gridSize=-9999.
INTEGER :: obsCarr=-9999
REAL :: obsLong=-9999.

!-----------------------------
! observation time
!-----------------------------
character(22) :: obsTime='XXXXXXXX'

!-------------------------------
! type of map (full or updated)
!-------------------------------
CHARACTER(2) :: mapType='XX'
double precision :: obsJulianDate=-9999.

!-----------------------------------------
! grid size of longitudes, sin-latitudes
!-----------------------------------------
REAL :: longStep, latStep

REAL :: lastObsCM

!----------------
! Missing value 
!----------------
REAL :: missingValue = 9999.9

END MODULE gong_parameters
