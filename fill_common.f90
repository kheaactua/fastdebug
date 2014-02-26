
! Used for the COMMON_INIT macro in model_macros_f.h
! As is, bounds checks don't work on the code it generates
! but aparently it's still right.
! According to Michel Valin, its a very dirty trick to 
! generate initialization code for common blocks
! with an unknown and variable number of elements using
! a macro generator.

subroutine fill_common(what,value,n)
	integer, dimension(-1:n) :: what
	what(1:n)=value
	return
end
