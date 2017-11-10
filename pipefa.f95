
 

!--------------------------------------------------------------------------
!  TITLE: CS260 Final Project: Pipe Flow Analysis
!  AUTHOR:Tanner Jones
!  CLASS: CSCI260A
!  DATE WRITTEN:Nov. 8
!  LAST REVISION:Nov. 8
!  DESCRIPTION: A Program used to analyse pipe flow. Not sure deets
!  VARIABLES USED:
!    NAME:           TYPE:     COMMENT:
!
!
!
!---------------------------------------------------------------------------
Program Pipefa
Implicit None  !Must explicitly declare all variables
! Declare the variables and initialize.
integer::function = 0, flowregime = 0
real::pipdiam, piplen, pipvel, piprough, reynolds, flow, friction, head



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
end subroutine headloss

subroutine flowrate()
implicit none
end subroutine flowrate

subroutine pipediam()
implicit none
end subroutine pipediam

End Program   !Pipefa
