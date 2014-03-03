
! Used for the COMMON_INIT macro in model_macros_f.h
! As is, bounds checks don't work on the code it generates
! but aparently it's still right.
! According to Michel Valin, its a very dirty trick to 
! generate initialization code for common blocks
! with an unknown and variable number of elements using
! a macro generator.

subroutine fill_common(what,value,n)
	IMPLICIT NONE
	integer, dimension(-1:n), intent(INOUT) :: what
	integer, INTENT(IN) :: n
	integer, intent(IN) :: value

	! Except the last two lines, this is all debug stuff

	integer :: i
	character(len=1024) :: tmp
	character(len=50) :: tmp2
	write(tmp, *) "In fill_common: value,n=", value,n, ", what="
	do i=-1,n
		write(tmp2,'(A,I2,A)') ", n=", i, ':'
		tmp=trim(tmp)//tmp2
		write(tmp2,*) what(i)
		tmp=trim(tmp)//tmp2
	enddo
	call fastdebug(tmp, __LINE__)

	what(1:n)=value
	return
end

! This is used for situations like,
! "do i=1,COMMON_SIZE(...)" which will trigger bounds errors
! (as this is part of GM's indexing tricks to try to access 
! the first element of a COMMON variable)
! So, replace the do loop with a call to this function
subroutine copy_with_offset_1(src,dest,n)
	IMPLICIT NONE

	integer, intent(IN), dimension(0:n) :: src
	integer, intent(OUT), dimension(1:n) :: dest
	integer, intent(IN) :: n

	dest(1:n) = src(1:n)

	return
endsubroutine copy_with_offset_1
