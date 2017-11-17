
 

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
logical::temp=.false.

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
               table%values(counter) = temp(2)
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
                case 1
                        CALL headloss(table)
                case 2
!			Call flowrate
                case 3
!			call pipediam	
                case default
                   write(*,*) "That is not a valid option"
 

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
character(len=20)::pipeType
character(len=50),dimension(2)::temp
logical::temp=.false.

type(roughnessTable) :: table


write(*,*) "Please enter pipe diameter, velocity, and length in that order:"
read(*,*) pipDiam, pipVel, pipLen
write(*,*) "Great, now we need the pipe type:"
do while temp==.FALSE.
     do counter=1,8
          if trim(table%types(counter))==trim(pipType)
               temp=.true.
               pipRough=table%values(counter)
          end if
     end do
     if temp==.false.
          write(*,*) "Please enter a valid type."
          write(*,*) table%types
     end if
end do
pipRough=pipRough/pipDiam
reynolds = (pipVel*pipDiam)/kinVisco
WRITE(*,*) "For 25 degrees C, our kinematic viscosity is ", kinVisco
WRITE(*,*) "Our relative roughness is ", pipRough


flow=pipVel*(pi*0.25*pipDiam*pipDiam)

IF (reynolds >= 4000)
     friction=0.25/(LOG((1/(3.7*(1/pipRough))) + (5.74/(reynolds**0.9)))**2)
ELSE IF (reynolds <= 2000) 
     friction=64/reynolds
ELSE
     WRITE(*,*) "Transitional Flow is not supported by this program"
end if

head=friction*(pipLen/pipDiam)*((pipVel**2)/(2*g))

write(*,*) "The head loss for the system is ", head

end subroutine headloss




subroutine flowrate()
implicit none
end subroutine flowrate






subroutine pipediam()
implicit none
end subroutine pipediam




End Program   !Pipefa
