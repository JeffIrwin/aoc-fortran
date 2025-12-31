
module convert_m

	use aoc_all_m
	use pips_io_m
	implicit none

contains

subroutine convert(input_filename, output_filename)
	character(len=*), intent(in) :: input_filename, output_filename
	!********
	character(len=:), allocatable :: d
	integer :: i
	type(str_vec_t) :: difficulties

	difficulties = new_str_vec()
	call difficulties%push("easy")
	call difficulties%push("medium")
	call difficulties%push("hard")

	do i = 1, i32(difficulties%len)
		d = difficulties%vec(i)%str
		! TODO: output filename doesn't work if it's in a subdir. Steal the
		! path-splitting fn (from syntran?) to put the difficulty prefix after
		! the dir but before the basename
		call write_pips_text(d//"-"//output_filename, read_pips_json(input_filename, d))
	end do

end subroutine convert

end module convert_m

program main
	use convert_m
	implicit none
	character(len=:), allocatable :: input_filename, output_filename

	print *, "Starting convert main"

	! TODO: check arg count, log help message
	input_filename = get_arg(1)
	print *, 'input_filename = "' // input_filename // '"'

	output_filename = get_arg(2)
	print *, 'output_filename = "' // output_filename // '"'

	call convert(input_filename, output_filename)

	print *, "Ending convert main"
end program main

