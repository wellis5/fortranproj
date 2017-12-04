
 

!--------------------------------------------------------------------------
!  TITLE: CS260 Final Project: Pipe Flow Analysis
!  AUTHOR:Tanner Jones
!  CLASS: CSCI260A
!  DATE WRITTEN:Nov. 8
!  LAST REVISION:Nov. 10
!  DESCRIPTION: A Program used to analyse pipe flow. Not sure deets
!  VARIABLES USED:
!    NAME:           TYPE:     COMMENT:
!
!
!
!
!
!
!---------------------------------------------------------------------------
Program Pipefa
Implicit None  !Must explicitly declare all variables


! Declare the variables and initialize.
integer::function = 0, flowRegime = 0, counter=0
real::pipDiam, pipLen, pipVel, pipRough, reynolds, flow, friction, head, kinVisco=0.0000008297, pi=3.14159265359, g=9.81
character(len=20)::pipeType
character(len=50),dimension(2)::temp
logical::templog=.false.

type roughnessTable
        character(len=20),dimension(8) :: types
        real,dimension(8) :: values
endtype roughnessTable

type(roughnessTable) :: table




!DON'T FORGET TO CHANGE UNDERSCORES TO SPACES AFTER READING IN FILE!!!
open(unit=100, file='table.txt', status='OLD', action='READ')
        do counter=0,7
               read(100,*) temp
               table%types(counter) = temp(1)
               read(trim(temp(2)), *) table%values(counter)
        end do

               


! Code the Civ Problem Here
do while (1==1)
        write(*,*) ":-------------------------------------------------------"
        write(*,*) ":-Please select the function you would like to use(1-3)-"
        write(*,*) ":---1) Determine head loss------------------------------"
        write(*,*) ":---2) Determine flow rate of water---------------------"
        write(*,*) ":---3) Calculate pipe diameter--------------------------"
        write(*,*) ":---4) Exit---------------------------------------------"
        write(*,*) ":-------------------------------------------------------"
        read(*,*) function
        select case (function)
                case (1)
                        CALL headloss(table)
                case (2)
			Call flowrate(table)
                case (3)
			call pipediam(table)	
                case (4)
                        exit
                case default
                   write(*,*) "That is not a valid option"
	end select 

end do
! Say Goodbye
   Write(*,*)
   Write(*,*) ' Have a Great Afternoon!!! '
   Write(*,*)


CONTAINS

subroutine headloss(table)
implicit none

integer::function = 0, flowRegime = 0, counter=0
real::pipDiam, pipLen, pipVel, pipRough, reynolds, flow, friction, head, kinVisco=0.0000008297, pi=3.14159265359, g=9.81
character(len=20)::pipType
character(len=50),dimension(2)::temp
logical::templog=.false.

type(roughnessTable) :: table


write(*,*) "Please enter pipe diameter, velocity, and length in that order:"
read(*,*) pipDiam, pipVel, pipLen
write(*,*) "Great, now we need the pipe type:"
do while (templog.EQV..FALSE.)
write(*,*) "Please enter the type:"
read(*,*) pipType
     do counter=1,8
          if (trim(table%types(counter))==trim(pipType)) then
               templog=.true.
               pipRough=table%values(counter)
          end if
     end do
     if (templog.EQV..false.) then
          write(*,*) "Please enter a valid type."
          write(*,*) table%types
     end if
end do
pipRough=pipDiam/pipRough
reynolds = (pipVel*pipDiam)/kinVisco
WRITE(*,*) "For 25 degrees C, our kinematic viscosity is ", kinVisco
WRITE(*,*) "Our relative roughness is ", pipRough


flow=pipVel*(pi*0.25*pipDiam*pipDiam)

IF (reynolds >= 4000) then
     friction=0.25/(LOG10((1/(3.7*(pipRough))) + (5.74/(reynolds**0.9)))**2)
ELSE IF (reynolds <= 2000) then 
     friction=64/reynolds
ELSE
     WRITE(*,*) "Transitional Flow is not supported by this program"
end if

head=friction*(pipLen/pipDiam)*((pipVel**2)/(2*g))

write(*,*) "The head loss for the system is ", head

end subroutine headloss





subroutine flowrate(table)

real::diameter, headloss
character(len=20)::pipetype
type(roughnessTable), intent(in)::table
real::flow
integer::err, i=0 
real::roughness,v
real::relrough, NR
real::f,A, length,temp  !friction factor
real, parameter::viscosity  = 0.0000008297
logical::templog=.false.




write(*,*) "We need the pipe type:"
do while (templog.EQV..FALSE.)
write(*,*) "Please enter the type:"
read(*,*) pipetype
     do counter=1,8
          if (trim(table%types(counter))==trim(pipetype)) then
               templog=.true.
               roughness=table%values(counter)
          end if
     end do
     if (templog.EQV..false.) then
          write(*,*) "Please enter a valid type."
          write(*,*) table%types
     end if
end do




WRITE(*,*)
	WRITE(*,*) 'What is the desired headloss?'
	READ(*,*) headloss
	WRITE(*,*)
	WRITE(*,*) 'what is the diameter of the pipe?'
	READ(*,*) diameter
	WRITE(*,*)
	WRITE(*,*)'what is the length of the pipe?'
	READ(*,*) length
	
	
!actual procedure starts here
! Calculating relative roughness

relrough = diameter/roughness

f = 0.01

A = sqrt((2*g*diameter*headloss)/length)

DO 
	temp=f
	
	v = A/(sqrt(f))
	
	NR = (v*diameter)/viscosity  	! calculated friction factor  
! do if statement to see if flow is laminar, turbulent or transitional if transition tell the user to go !away
	IF (NR <= 2000) THEN  !flow is laminar
		f = 64./NR
	ELSE IF(NR > 2000 .AND. NR < 4000)THEN  ! flow is trasnssitional
		WRITE(*,*) 'flow is transitional, cannot calculate flow'
		STOP
	ELSE
		f=0.25/(LOG10((1/(3.7*(relrough))) + (5.74/(NR**0.9)))**2)
	END IF
	
	IF( abs(temp-f)/f <= 0.01) EXIT
	
	
END DO
! now we have the value for the f
! now solve for the velocity

v = A/(sqrt(f))

! now we solve for the flow 

flow  = v*3.14159*(diameter**2)
write(*,*) "The flowrate is ", flow




end subroutine flowrate






subroutine pipediam(table)
implicit none



REAL::f,length,q,headloss,roughness, relrough,D, temp, velocity, NR, A
CHARACTER(len=20)::pipeType
real, parameter::viscosity  = 0.0000008297
type(roughnessTable)::table
logical::templog=.false.
write(*,*) "First we'll get the pipe roughness."
do while (templog.EQV..FALSE.)
Write(*,*) "Please enter pipe type"
read(*,*) pipeType
     do counter=1,8
          if (trim(table%types(counter))==trim(pipeType)) then
               templog=.true.
               pipRough=table%values(counter)
          end if
     end do
     if (templog.EQV..false.) then
          write(*,*) "Please enter a valid type."
          write(*,*) table%types
     end if
end do

WRITE(*,*) 'What is the flow (m^3/s)?'
READ(*,*) q
WRITE(*,*) 'What is the head loss?'
READ(*,*) headloss
WRITE(*,*) 'What is the pipe length'
READ(*,*) length
relrough = D/roughness

!Assume a friction factor of 0.01
f = 0.01
DO
	temp = f
	! calculated diameter
	D = ((8*length*q**2)/(headloss*9.81*pi**2))**0.2
	! caluculate relative roughness
	relrough = D/roughness
	velocity = q/(0.25*pi*D**2)
	NR = velocity*D/viscosity
	
	f=0.25/(LOG10((1/(3.7*(relrough))) + (5.74/(NR**0.9)))**2)
	IF( abs(temp-f)/f <= 0.01) EXIT
	
END DO
	WRITE(*,*) 'diameter = ',D,'meters'
write(*,*)
write(*,*)
write(*,*)
END SUBROUTINE pipediam

End Program   !Pipefa
