FUNCTION read_gong_pre_header (unit) ! input

USE gong_parameters
USE space_cal_routines
IMPLICIT NONE

!-----------------------
! Declare function type
!-----------------------
INTEGER :: read_gong_pre_header

!--------------------------------------
! Read in data, do any reversals, etc
!--------------------------------------
integer, intent(in) :: unit ! unit # of input file

! --------------------------------------------------------------------
! Local variables
! --------------------------------------------------------------------
character(10) :: mapDate = 'XXXX'
character(8) :: mapTime = 'XXXX'

REAL :: nowCM=-9999.

integer :: status  = 0 !  status of subroutine call

integer :: nfound  = 0 ! for reading fits file

integer :: naxes(2) ! dimension of data in fits file

character(40) :: comment

integer :: year, month, day, hours, minutes, seconds
character(4) :: strYear
character(2) :: strMonth, strDay, strHours, strMinutes, strSeconds
integer :: length
character(2) :: lead_zero


! begin code ==========================================================
! initialize data
naxes = 0

! THIS MUST BE SET TO 0, OR MIGHT GET STRANGE RESULTS*********
status = 0

!  Determine the size of the image.
call ftgknj(unit, 'NAXIS', 1, 2, naxes, nfound, status)

print *,'read_gong_pre_header : nfound = ',nfound
print *,'read_gong_pre_header : naxes = ',naxes
print *,'read_gong_pre_header : status = ',status

if ((naxes(1) > 1000) .or. (naxes(2) > 1000)) then
     print *,'Something wrong with fits file reading.'
     print *,'Recompile w/    COMPILE = g95 -c -i4      in the make file'
     !print *,'COMPILE = g95 -c -i4'
     !print *,'in the make file'
end if

! Check that it found both NAXIS1 and NAXIS2, and NAXIS3 keywords.
if (nfound /= 2) then
    print *,'read_gong_pre_header : READIMAGE failed to read the NAXISn keywords.'
    stop
end if

!----------------------
! Size of data arrays
!----------------------
iDim = naxes(1)
jDim = naxes(2)

print *, 'read_gong_pre_header : iDim, jdim = ',iDim, jdim


!  Initialize variables
!npixels = naxes(1)*naxes(2)
!-------------------------------------------
! READ IN DATA
!--------------------------------------------

! The STATUS parameter must always be initialized.


!-------------------------------------------
! read integer keyword CARROT from fits file
!-------------------------------------------
status = 0
call ftgkyj(unit, 'CAR_ROT', carrRotation, comment, status)
!print *, 'after ftgkyj, status = ', status, ' carrRotation = ',carrRotation
if (status /= 0) then
    print *,'read_gong_pre_header : after ftgkyj ..........................status = ',status
    print *,'read_gong_pre_header : Error reading CAR_ROT, STOPPING'
    STOP
end if

!-----------------------
! carrington longitude
!-----------------------
status = 0
call ftgkye(unit, 'LONG0', carrLong, comment, status)
!print *, 'after ftgkye, status = ',status,' carrLong = ',carrLong
if (status /= 0) then
    print *,'read_gong_pre_header : after ftgkye ..........................status = ',status
    print *,'read_gong_pre_header : Error reading LONG0, STOPPING'
    STOP
end if

!-----------------------
! longitude grid size
!-----------------------
status = 0
call ftgkye(unit, 'CDELT1', longStep, comment, status)
if (status /= 0) then
    print *,'read_gong_pre_header : after ftgkye ..........................status = ',status
    print *,'read_gong_pre_header : Error reading CDELT1, STOPPING'
    STOP
end if

!-----------------------
! sin-latitude grid size
!-----------------------
status = 0
call ftgkye(unit, 'CDELT2', latStep, comment, status)
if (status /= 0) then
    print *,'read_gong_pre_header : after ftgkye ..........................status = ',status
    print *,'read_gong_pre_header : Error reading CDELT2, STOPPING'
    STOP
end if


!status = 0
!call ftgkye(unit, 'CR60', lastObsCM, comment, status)
!if (status /= 0) then
!    print *,'read_gong_pre_header : after ftgkye ..........................status = ',status
!    print *,'read_gong_pre_header : Error reading CR60, STOPPING'
!    STOP
!end if
!print *,'read_gong_pre_header : lastObsCM = ',lastObsCM


status = 0
call ftgkys(unit, 'MAPDATE', mapDate, comment, status)
if (status /= 0) then
    print *,'read_gong_pre_header : after ftgkys ..........................status = ',status
    print *,'read_gong_pre_header : Error reading MAPDATE, STOPPING'
    STOP
end if
print *,'read_gong_pre_header : mapDate = ',mapDate

status = 0
call ftgkys(unit, 'MAPTIME', mapTime, comment, status)
if (status /= 0) then
    print *,'read_gong_pre_header : after ftgkys ..........................status = ',status
    print *,'read_gong_pre_header : Error reading MAPTIME, STOPPING'
    STOP
end if
print *,'read_gong_pre_header : mapTime = ',mapTime

obsTime = trim(mapDate)//'_'//trim(maptime)
print *,'read_gong_pre_header : obsTime = ',obsTime

!---------------------------------------------------
! Get julian date from CRNOW of last map
! (Not from CR60)
!---------------------------------------------------
!obsCarr = FLOOR(nowCM)
!obsLong = nowCM - obsCarr
!obsLong = (1. - obsLong)*360.
!print *,'convert_syn_gong : obsCarr, obsLong = ', obsCarr, obsLong



!---------------------------------------------------
! Get observation date & time from the header
!---------------------------------------------------
if (maptype /= 'FR') then

    status = 0
    call ftgkye(unit, 'CRNOW', nowCM, comment, status)
    if (status /= 0) then
        print *,'read_gong_pre_header : after ftgkye ..........................status = ',status
        print *,'read_gong_pre_header : Error reading CRNOW, STOPPING'
        STOP
    end if
    !print *,'read_gong_pre_header : nowCM = ',nowCM

    obsCarr = FLOOR(nowCM)

    !--------------------------
    ! Get longitude in degrees
    !--------------------------
    obsLong = nowCM - obsCarr
    obsLong = (1. - obsLong)*360.

else 

    !---------------------------------------------
    ! Set observation date of full maps to middle
    !---------------------------------------------
    obsCarr = carrRotation
    obsLong = 180.0

end if


CALL carr_jd(real(obsCarr), obsLong, obsJulianDate)
obsJulianDate = 2440000.0 + obsJulianDate
!print *,'read_gong_pre_header : obsJulianDate = ', obsJulianDate


! Get string date from obsJulianDate
call caldat(obsJulianDate, year, month, day, hours, minutes, seconds)
!print *,'read_gong_pre_header : year, month, day, hours, minutes, seconds = '
!print *,'read_gong_pre_header : ', year, month, day, hours, minutes, seconds

!------------------
! make string date 
!------------------
! Convert integer to character ::
!----------------------------------
write(strYear, '(I4)') year
write(strMonth, '(I2)') month
write(strDay, '(I2)') day
write(strhours, '(I2)') hours
write(strminutes, '(I2)') minutes
write(strseconds, '(I2)') seconds

strMonth = lead_zero(strMonth,2)
strDay = lead_zero(strDay,2)
strHours = lead_zero(strHours,2)
strMinutes= lead_zero(strMinutes,2)
strSeconds = lead_zero(strSeconds,2)

obsTime = stryear//':'//strmonth//':'//strday//'_'//strhours//'h:'//strMinutes//'m:'//strSeconds//'s'
print *,'read_gong_pre_header : obsTime = ',obsTime 

!-------------------
! set return value
!-------------------
read_gong_pre_header = 0

end FUNCTION read_gong_pre_header
