PROGRAM convert_syn_gong

! MAIN PROGRAM DOCUMENTATION BLOCK
!
! Programmer : Leslie Mayer
!
! Abstract : Read a gong synoptic file, convert to format 
!            needed for WSA runs
! Usage : main
!  Input Argument List : 
!     Namelist file
!  Output Argument List :
!     None
!  Subprograms called :
!     Utilities :
!     Library :
!  
! Attributes :
!  Language : Fortran 90
!  Machine :
!
!
!================================================================
! 
! > convert_syn_gong
!================================================================

USE fits_files
USE gong_parameters
IMPLICIT NONE


!NAMELIST : inputs ----------------------------------------
character(100) :: inputDir
character(80) :: inputFile
character(100) :: outputDir
!character(2) :: mapType = 'XX'   this is in gong_parameters
!----------------------------------------------------------

! Value of missing data
!real, parameter :: missingValue = 9999.9  this is in gong_parameters

character(120) :: inputName, outputName
character(40), PARAMETER :: outputNameFile = 'preoutlist.txt'
integer, PARAMETER :: outNameUnit = 12
integer :: openStatus

!-------------------------------
! name of namelist file to read
!-------------------------------
character(50) :: nameListFile='xxxxxxx'

integer :: inUnit, outUnit  ! unit numbers
integer :: write_gong_syn_header !  declare what type of function this is
integer :: status=0, group=0, fpixel=0, nelements=0

integer :: funcStatus ! status of function return
integer :: read_gong_pre_header !  declare what type of function this is

character(4) :: strRot
character(6) :: strLong ! for making output filename
! for making outputName
character(6) :: lead_zero  ! function

!----------------------------------------------
! Alocatable arrays for holding the fits data
!----------------------------------------------
real, dimension(:,:), allocatable  :: data  ! (iDim,jDim)
integer :: allocateStatus

! NEED THIS B/C of allocatable data array *********************
INTERFACE
     SUBROUTINE get_2dim_data (unit,  i_dim, j_dim,  & ! input
                               data)  ! output
     integer, intent(in) :: unit ! unit # of input file
     integer, intent(in) :: i_dim ! RESOLUTION IN X
     integer, intent(in) :: j_dim
     real, intent(out), dimension(:,:) :: data
     END SUBROUTINE get_2dim_data
END INTERFACE

NAMELIST /inputs/ mapType,  &
                  inputDir, inputFile, outputDir

! BEGIN CODE ======================================================================
print *,'Input name of namelist file to read : '
read(*, '(A50)' ) nameListFile
print *,'convert_syn_gong : nameListFile = ',nameListFile


!-------------------------
! OPEN THE NAMELIST FILE
!-------------------------
OPEN (UNIT=16, FILE=trim(nameListFile), STATUS='OLD')
read (16, NML=inputs)


PRINT *, 'convert_syn_gong : inputFile = ', inputFile
PRINT *, 'convert_syn_gong : inputDir = ', inputDir

inputName = trim(inputDir)//trim(inputFile)
PRINT *, 'convert_syn_gong : inputName = ', inputName

!----------------------
!  Open the FITS file 
!----------------------
inUnit =  open_fits_read(inputname)

!-------------------------------------------------
! Get header information from synoptic input file
!-------------------------------------------------
funcStatus =  read_gong_pre_header (inUnit) ! input

!-------------------------------------------------
! Check carrington longitude to make sure not 0
!-------------------------------------------------
!print *,'convert_syn_gong : carrRotation = ', carrRotation
if (carrRotation <= 0) then
    print *,'convert_syn_gong : carrRotation = ', carrRotation, ' STOPPING......................'
end if

!-------------------------
! Set version number to 1
!-------------------------
carrVersion = 1


!-------------------
! Get data values
!-------------------
!---------------------
! allocate data array
!---------------------
allocate(data(iDim,jDim), stat = Allocatestatus)
if (Allocatestatus /= 0) stop "convert_syn_gong : couldn't allocate data array"

!----------------------
! initialize data to 0
!----------------------
data = 0.

!--------------------------------
! Read 2-d data 
!--------------------------------
CALL get_2dim_data (inUnit, iDim, jDim,  & ! input
                    data)  ! output

!print *,'convert_syn_gong : data(110,10) = ', data(110,10)
!print *,'convert_syn_gong : data(144,72) = ', data(144,72)

!---------------------
! CHECK FOR DATA GAPS
!---------------------
!index = where((data ge missing) and (data le -1.*missing) , count) 
!if (index[0] ne -1) then begin
!    data[index] = missing
!    print,'convert_syn_gong : there is missing data in ',file
!endif
WHERE ( (data >= (missingValue - 1.)) .and. (data <= -1.*missingValue) ) 
       data = missingValue
END WHERE

outputDir = trim(outputDir)

! old way, based on input file name
!outputName = 'syn'//inputFile(4 : len_trim(inputFile)-3)//'fts'
!outputName = 'syn'//inputFile(4 : len_trim(inputFile)-6)//'fits'

!----------------------------------
! Convert integer to character ::
!----------------------------------
write(strRot, '(I4)') carrRotation
write(strLong, '(F6.2)') carrLong

strLong = lead_zero(strLong,6)

! new way, based on rotation & longitude
outputName = 'syn_'//strRot//'_'//strLong//'_'//'01_gong.fits'


!--------------------------------------
! Write name of output file to a file
!--------------------------------------
OPEN (UNIT=outNameUnit, FILE=outputNameFile, IOSTAT=OpenStatus)
if (OpenStatus /= 0) then
     print *,'convert_syn_gong : Problem opening ',outputNameFile
     print *,'convert_syn_gong : OpenStatus = ', OpenStatus
     print *,'convert_syn_gong : STOPPING .............'
     STOP
end if

write(outNameUnit, '(A)') trim(outputName)
CLOSE (outNameUnit)


outputName = trim(outputDir)//trim(outputName)
!print *,'convert_syn_gong : outputName = ', outputName

!-------------------------------------------------------------------
! Open file for writing out fits file 
!-------------------------------------------------------------------
outUnit = open_fits_write(outputName)

!-------------------------------------------
! Write out re-processed synoptic map header
!-------------------------------------------
funcstatus = write_gong_syn_header (outUnit, iDim, jDim)

status = 0
group = 1
fpixel = 1
nelements = iDim*jDim
call ftppre(outUnit, group, fpixel, nelements, data, status)

!-------------------------------------------------------------------
!  The FITS file must always be closed before exiting the program. 
!  Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
!-------------------------------------------------------------------
status = 0
call ftclos(outUnit, status)
status = 0
call ftfiou(outUnit, status)


!-------------------------------------------
! Create file to let perl script know that 
! this program ran successfully
!-------------------------------------------
OPEN (UNIT=57, FILE='goodRun.dat')
close(57)

end PROGRAM convert_syn_gong
