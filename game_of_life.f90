program game_of_life
! =====================
! conway's game of life
!
! =====================

implicit none

! Declare
integer, parameter :: nrow = 31, ncol = 31
integer :: i,j,t, sum, num_gens, mod_val
integer, dimension(nrow,ncol) :: board, tempBoard
integer, dimension(8) :: values

logical :: call_once = .false.

! Choose start model
! 1=sm expoder, 2=ten cell row, 3=exploder, 4=random movements
integer :: model = 3, speed = 250

! Initial setup
if (model == 1) then
   board(:,:) = 0
   board(15,16) = 1
   board(16,15) = 1
   board(16,16) = 1
   board(16,17) = 1
   board(17,15) = 1
   board(17,17) = 1
   board(18,16) = 1
elseif (model == 2) then
   board(:,:) = 0
   board(16,12) = 1
   board(16,13) = 1
   board(16,14) = 1
   board(16,15) = 1
   board(16,16) = 1
   board(16,17) = 1
   board(16,18) = 1
   board(16,19) = 1
   board(16,20) = 1
   board(16,21) = 1
elseif (model == 3) then
   board(:,:) = 0
   board(14,14) = 1
   board(15,14) = 1
   board(16,14) = 1
   board(17,14) = 1
   board(18,14) = 1
   board(14,16) = 1
   board(18,16) = 1
   board(14,18) = 1
   board(15,18) = 1
   board(16,18) = 1
   board(17,18) = 1
   board(18,18) = 1
elseif (model == 4) then
   board(:,:) = 0
   do i=1,5
      do j=1,5
         board(i*3,j)=1
         board(i*3, j+1) = 1
         board(i*3, j+2) = 1
      enddo
   enddo
else
   board(:,:) = 0
   board(16,13) = 1
   board(16,14) = 1
   board(16,15) = 1
   board(16,16) = 1
   board(16,17) = 1
   board(16,18) = 1
   board(16,19) = 1
   board(16,21) = 1

end if


tempBoard = 0


call system ("clear")

do while(.true.)
   ! animation loop
   call date_and_time(VALUES=values)   
   mod_val = mod(values(8), speed)
   if ( (mod_val == 0) .and. (call_once .eqv. .false.) ) then
      call_once = .true.
      call calculate_board(board, nrow,ncol,tempBoard)
      board = tempBoard
      call draw_board(board, nrow,ncol)

   elseif ( (call_once .eqv. .true.) .and. (mod_val == 0) ) then
      call_once = .true.
      
   else
      call_once = .false.
   end if
enddo

contains
  subroutine calculate_board(b, r,c,tB)
    integer :: r, c
    integer, dimension(r,c) :: b, tB
    do i=2, r-1
       do j=2, c-1
          sum = 0
          sum = b(i-1, j-1) + b(i, j-1) + b(i+1, j-1) ! 
          sum = sum + b(i-1, j) + b(i+1, j)
          sum = sum + b(i-1, j+1) + b(i, j+1) + b(i+1, j+1)
          if(b(i,j).eq.1 .and. sum.le.1) then
             tB(i,j) = 0
          elseif(b(i,j).eq.1 .and. sum.le.3) then
             tB(i,j) = 1
          elseif(b(i,j).eq.1 .and. sum.ge.4)then
             tB(i,j) = 0
          elseif(b(i,j).eq.0 .and. sum.eq.3)then
             tB(i,j) = 1
          endif
       enddo
    enddo
    
    return 
  end subroutine calculate_board

  subroutine draw_board(b,r, c)
    integer :: r, c
    integer, dimension(r,c) :: b
    character(r) :: output
    call system ("clear")
    do i=1, nrow
       output = ""
       do j=1, ncol
          if (board(i,j) == 1) then
             output = trim(output)//"#"
          else
             output = trim(output)//"."
          endif
       enddo
       print *, output
    enddo
  end subroutine draw_board

end program
