
 

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
real::pipDiam, pipLen, pipVel, pipRough, reynolds, flow, friction, head
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
!			headloss
                case 2
!			flowrate
                case 3
!			pipediam	
                case default
                   write(*,*) "That is not a valid option"
 

end do
! Say Goodbye
   Write(*,*)
   Write(*,*) ' Have a Great Afternoon!!! '
   Write(*,*)

subroutine headloss()
implicit none
write(*,*) "Please enter pipe diameter, velocity, and length in that order:"
read(*,*) pipeDiam, pipeVel, pipeLen
write(*,*) "Great, now we need the pipe type:"
do while temp==.FALSE.
     do counter=0,7
          if trim(table%types(counter))==trim(pipType)
               temp=.true.
               pipRough=table%values(counter)
          end if
     end do
     if temp==.false.
          write(*,*) "Please enter a valid type."
     end if
end do



end subroutine headloss

subroutine flowrate()
implicit none
end subroutine flowrate

subroutine pipediam()
implicit none
end subroutine pipediam

End Program   !Pipefa
