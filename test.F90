program test

	implicit none
	integer :: ierr
	character(len=200) :: tmpstr

	CALL MPI_Init(ierr)

	print *,"Calling fastdebug"
	call fastdebug("Called from fortran", __LINE__);
	write(tmpstr, '(A)') 'Writing to a long string'
	call fastdebug(tmpstr, __LINE__);
	call fastdebug("Called a second time from fortran", __LINE__);

	CALL MPI_Finalize(ierr);

endprogram test
