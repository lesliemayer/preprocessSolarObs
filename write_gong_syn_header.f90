FUNCTION write_gong_syn_header (unit, i_new, j_new) ! input

USE gong_parameters
IMPLICIT NONE

!-----------------------
! Declare function type
!-----------------------
INTEGER :: write_gong_syn_header

!--------------------------------------
! Read in data, do any reversals, etc
!--------------------------------------
integer, intent(in) :: unit ! unit # of input file
integer, intent(in) :: i_new, j_new

!----------------
! for fits output
!----------------
logical :: simple, extend
integer :: status=0, blocksize=0, bitpix=0, naxis=0, naxes(2)

! Function for reading wsa version #
character(12), external :: get_wsavers
character(12) :: version

!=======================================================================
! Begin Code
!=======================================================================
!------------------------------------------------------------------
!  Initialize parameters about the FITS image.
!  BITPIX = 16 means that the image pixels will consist of 16-bit
!  integers.  The size of the image is given by the NAXES values. 
!  The EXTEND = TRUE parameter indicates that the FITS file
!  may contain extensions following the primary array.
!------------------------------------------------------------------
simple = .true.
bitpix = -32  ! -32 for IEEE single precision floating point
naxis = 2
naxes(1) = i_new
naxes(2) = j_new
!naxes(3) = 
extend = .false.

!------------------------------------------------
!  Write the required header keywords to the file
!------------------------------------------------
status = 0 
call ftphpr(unit, simple, bitpix, naxis, naxes, 0, 1, extend, status)
if (status /= 0) then
   print *,'write_gong_syn_header : after FTPHPR ..........................status = ', status
   print *,'write_gong_syn_header : Error writing naxes to header, STOPPING'
   STOP
end if

!------------------
! Add today's date
!------------------
status = 0
call ftpdat(unit, status)
if (status /= 0) then
    print *,'write_gong_syn_header : after FTPDAT ..........................status = ',status
    print *,'write_gong_syn_header : Error adding todays date, STOPPING'
    STOP
end if

!----------------------------------------------------
! add observatory name
!----------------------------------------------------
status = 0
call ftpkys(unit, 'OBSER', obsName, 'Observatory', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after ftpkys OBSER..........................status = ',status
    print *,'write_gong_syn_header : Error adding observatory name, STOPPING'
    STOP
end if

!----------------------------------------------------
! add rotation
!----------------------------------------------------
status = 0
call ftpkyj(unit, 'CARROT', carrRotation, 'Carr. Rot. of leading edge of map', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after ftpkyj CARROT..........................status = ',status
    print *,'write_gong_syn_header : Error adding rotation number, STOPPING'
    STOP
end if


!----------------------------------------------------
! add longitude
!----------------------------------------------------
status = 0
call ftpkyf(unit, 'CARRLONG', carrLong, 2, 'Long. of leading edge of map', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after FTPKYf  CARRLONG ..........................status = ',status
    print *,'write_gong_syn_header : Error adding carrington longitude, STOPPING'
    STOP
end if

!----------------------------------------------------
! add carrington longitude version
!----------------------------------------------------
status = 0
call ftpkyj(unit, 'CARRVERS', carrVersion, 'Longitude Version number', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after FTPKYj CARRVERS..........................status = ',status
    print *,'write_gong_syn_header : Error adding version number, STOPPING'
    STOP
end if


!----------------------------------------------------
! add maptype
!----------------------------------------------------
status = 0
call ftpkys(unit, 'MAPTYPE', maptype, 'Type of Map', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after ftpkys MAPTYPE, status = ', status
    print *,'write_gong_syn_header : Error adding type of map, STOPPING'
    STOP
end if

!----------------------------------------------------
! add longitudinal step size
!----------------------------------------------------
status = 0
call ftpkyf(unit, 'LONSTEP', longStep, 2, 'Carrington long. step (deg)', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after FTPKYf LONSTEP..........................status = ',status
    print *,'write_gong_syn_header : Error adding Longitude Step Size, STOPPING'
    STOP
end if

!----------------------------------------------------
! add latitudinal step size
!----------------------------------------------------
!    fxaddpar, header, 'SLSTEP', latstep, 'Sine-lat step'
status = 0
call ftpkyf(unit, 'SLSTEP', latStep, 7, 'Sine-lat step', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after FTPKYf SLSTEP..........................status = ',status
    print *,'write_gong_syn_header : Error adding Latitude Step Size, STOPPING'
    STOP
end if

!----------------------------------------------------
! add observation julian date
!----------------------------------------------------
status = 0
call ftpkyg(unit, 'OBSJUL', obsJulianDate, 4, 'Obs. Julian Date of latest magnetogram (CM)', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after ftpkyg : OBSJUL'
    print *,'write_gong_syn_header : Error adding Obs. Julian Date of latest magnetogram, STOPPING'
    STOP
end if

!----------------------------------------------------
! add observation carrington
!----------------------------------------------------
status = 0
call ftpkyj(unit, 'OBSCAR', obsCarr, 'Carr. Rot. of latest magnetogram (CM)', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after FTPKYj OBSCAR..........................status = ',status
    print *,'write_gong_syn_header : Error adding Carr. Rot. of latest magnetogram, STOPPING'
    STOP
end if

!----------------------------------------------------
! add observation longitude
!----------------------------------------------------
status = 0
call ftpkyf(unit, 'OBSLON', obsLong, 2, 'Long. of latest magnetogram (CM)', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after FTPKYf OBSLON..........................status = ',status
    print *,'write_gong_syn_header : Error adding Long. of latest magnetogram, STOPPING'
    STOP
end if

!----------------------------------------------------
! add observation time
!----------------------------------------------------
status = 0
call ftpkys(unit, 'OBSTIME', obsTime, 'Obs. time of latest magnetogram (CM)', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after FTPKYs OBSTIME..........................status = ',status
    print *,'write_gong_syn_header : Error adding Obs. time of latest magnetogram, STOPPING'
    STOP
end if

!--------------------
! Add WSA version #
!--------------------
version = get_wsavers()
status = 0
call ftpkys(unit, 'PROVERS', version, 'WSA version #', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after ftpkys  status PROVERS..........= ', status
    print *,'write_gong_syn_header : Error adding WSA version #, STOPPING'
    STOP
end if

!----------------------------------------------------
! add missing value
!----------------------------------------------------
status = 0
call ftpkyf(unit, 'MISSING', missingValue, 2, 'Value of missing data', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after FTPKYf MISSING..........................status = ',status
    print *,'write_gong_syn_header : Error adding missingValue, STOPPING'
    STOP
end if

!    fxaddpar, header, 'COMMENT', ' *** Data re-processed from original gong data ***'
status = 0
call ftpkys(unit, 'COMMENT', ' *** Data re-processed from original gong data ***', '', status)
if (status /= 0) then
    print *,'write_gong_syn_header : after ftpkys COMMENT************ status = ', status
    print *,'write_gong_syn_header : Error adding WSA version #, STOPPING'
    STOP
end if

write_gong_syn_header = 0

end FUNCTION write_gong_syn_header
