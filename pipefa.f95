
 

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
!               read(trim(temp(2)), *) table%values(counter)
        end do

               


! Code the Civ Problem Here
do while (1==1)
        write(*,*) ":-------------------------------------------------------"
        write(*,*) ":-Please select the function you would like to use(1-3)-"
        write(*,*) ":---1) Determine head loss------------------------------"
        write(*,*) ":---2) Determine flow rate of water---------------------"
        write(*,*) ":---3) Calculate pipe diameter--------------------------"
        write(*,*) ":-------------------------------------------------------"
        read(*,*) function
        select case (function)
                case (1)
                        CALL headloss(table)
                case (2)
!			Call flowrate
                case (3)
!			call pipediam	
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
pipRough=pipRough/pipDiam
reynolds = (pipVel*pipDiam)/kinVisco
WRITE(*,*) "For 25 degrees C, our kinematic viscosity is ", kinVisco
WRITE(*,*) "Our relative roughness is ", pipRough


flow=pipVel*(pi*0.25*pipDiam*pipDiam)

IF (reynolds >= 4000) then
     friction=0.25/(LOG((1/(3.7*(1/pipRough))) + (5.74/(reynolds**0.9)))**2)
ELSE IF (reynolds <= 2000) then 
     friction=64/reynolds
ELSE
     WRITE(*,*) "Transitional Flow is not supported by this program"
end if

head=friction*(pipLen/pipDiam)*((pipVel**2)/(2*g))

write(*,*) "The head loss for the system is ", head

end subroutine headloss





subroutine flowrate(table, flow)

real::diameter, headloss
character(len=20)::pipetype
type(roughnessTable), intent(in)::table
real, intent(out)::flow
integer::err, i=0 
real::roughness,v
real::relrough, NR
real::f,A, length,temp  !friction factor
real, parameter::viscosity  = 0.0000008297

do
WRITE(*,*) 'please enter the type of pipe'
READ(*,*) pipetype

DO i=1,8

        IF(table%types(i) == pipetype)THEN
              roughness = table%values(i)
	ELSE
		WRITE(*,*)'plastic'
		WRITE(*,*)'drawn'
		WRITE(*,*)'steel'
		WRITE(*,*)'galvanized_iron'
		WRITE(*,*)'coated_iron'
		WRITE(*,*)'uncoated_iron'
		WRITE(*,*)'concrete'
		WRITE(*,*)'riveted_steel'
		WRITE(*,*)'Enter pipe type from the above table.'
		READ(*,*) pipetype
	END IF
	
END DO
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
	f = temp
	
	v = A/(sqrt(f))
	
	NR = (v*diameter)/viscosity  	! calculated friction factor  
! do if statement to see if flow is laminar, turbulent or transitional if transition tell the user to go !away
	IF (NR <= 2000) THEN  !flow is laminar
		f = 64./NR
	ELSE IF(NR > 2000 .AND. NR < 4000)THEN  ! flow is trasnssitional
		WRITE(*,*) 'flow is transitional, cannot calculate flow'
		STOP
	ELSE
		f=0.25/(LOG((1/(3.7*(1/relrough))) + (5.74/(NR**0.9)))**2)
	END IF
	
	IF( abs(temp-f)/f <= 0.01) EXIT
	
	
END DO
! now we have the value for the f
! now solve for the velocity

v = A/(sqrt(f))

! now we solve for the flow 

flow  = v*3.14159*(diameter**2)




end subroutine flowrate






subroutine pipediam(table)
implicit none
integer::function = 0, flowRegime = 0, counter=0
real::pipDiam, pipLen, pipVel, pipRough, reynolds, flow, friction, head, kinVisco=0.0000008297, pi=3.14159265359, g=9.81
character(len=20)::pipType
character(len=50),dimension(2)::temp
logical::templog=.false.

type(roughnessTable) :: table



write(*,*) "First we'll get the pipe roughness."
do while (templog.EQV..FALSE.)
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



end subroutine pipediam




End Program   !Pipefa
