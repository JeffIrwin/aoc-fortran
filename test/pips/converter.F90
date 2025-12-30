
module converter_m

	use json_module
	implicit none

contains

subroutine json_hello_world()
	type(json_file) :: json

	call json%deserialize('{"name": "Leonidas"}')

	!print *, "json.name = ", json%get("name")
	call json%print()

end subroutine json_hello_world

subroutine convert(input_filename, output_filename)
	use utils_m
	character(len=*), intent(in) :: input_filename, output_filename
	!********
	integer :: nr, nd, ni, ir, nx, ny, x, y, ii
	type(json_file) :: json
	character(len=:), allocatable :: rkey, ikey, type_

	call json%load(input_filename)

	!call json%print()
	!call json%

	! TODO: refactor as a subroutine to share with easy, medium, and hard
	! puzzles all in one input json

	! TODO: move down with domino parsing TBD
	call json%info("easy.dominoes", n_children = nd)
	print *, "nd = ", nd

	nx = -huge(nx)
	ny = -huge(ny)

	call json%info("easy.regions", n_children = nr)
	print *, "nr = ", nr
	do ir = 1, nr
		! Iterate over regions

		rkey = "easy.regions["//to_str(ir)//"]"
		print *, "rkey = ", rkey

		call json%get(rkey//".type", type_)
		print *, "type_ = ", type_

		call json%info(rkey//".indices", n_children = ni)
		print *, "ni = ", ni

		!call json%get("easy.regions["//to_str(ir)//"]", )
		do ii = 1, ni
			ikey = rkey//".indices["//to_str(ii)//"]"
			call json%get(ikey//"[1]", x)
			call json%get(ikey//"[2]", y)
			print *, "x, y = ", x, y

			nx = max(nx, x)
			ny = max(ny, y)
		end do
	end do
	nx = nx + 1
	ny = ny + 1
	print *, "nx, ny = ", nx, ny

end subroutine convert

end module converter_m

program main
	use converter_m
	implicit none
	character(len=:), allocatable :: input_filename, output_filename
	character :: buffer*1024

	print *, "Starting converter main"

	call json_hello_world()

	call get_command_argument(1, value = buffer)!, status = io) ! TODO: just use args.F90
	input_filename = trim(buffer)
	print *, 'input_filename = "' // input_filename // '"'

	call get_command_argument(2, value = buffer)!, status = io) ! TODO: just use args.F90
	output_filename = trim(buffer)
	print *, 'output_filename = "' // output_filename // '"'

	call convert(input_filename, output_filename)

	print *, "Ending converter main"
end program main

