
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
