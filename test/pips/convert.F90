
module convert_m

	use aoc_all_m
	use utils_m
	use pips_io_m
	implicit none

contains

subroutine convert(input_filename, output_filename)
	character(len=*), intent(in) :: input_filename, output_filename
	!********
	character(len=:), allocatable :: difficulty, dir, base
	integer :: i
	type(str_vec_t) :: difficulties
	type(pips_t) :: pips

	difficulties = new_str_vec()
	call difficulties%push("easy")
	call difficulties%push("medium")
	call difficulties%push("hard")

	dir = get_dir(output_filename)
	base = get_base_with_ext(output_filename)

	do i = 1, i32(difficulties%len)
		difficulty = difficulties%vec(i)%str
		pips = read_pips_json(input_filename, difficulty)
		call write_pips_text(dir//difficulty//"-"//base, pips)
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

