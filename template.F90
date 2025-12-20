
module aoc_m
	use iso_fortran_env
	use utils
	implicit none
contains

!===============================================================================

function part1(filename) result(ans_)
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: ans_
	!********

	integer :: iu, io, sum_
	character(len = :), allocatable :: str_

	sum_ = 0
	open(newunit = iu, file = filename, action = "read")
	do
		str_ = read_line(iu, io)
		if (io /= 0) exit
		print *, "str_ = ", str_
	end do
	close(iu)

	write(*,*) "part 1 = ", sum_
	ans_ = to_str(sum_)

end function part1

!===============================================================================

end module aoc_m

!===============================================================================

program main
	use aoc_m
	use args_m
	implicit none

	character(len = :), allocatable :: p1, p2, expect1, expect2
	logical :: do_p1, do_p2, error = .false.
	type(args_t) :: args

	args = parse_args()
	write(*,*) fg_bright_magenta//"Starting Fortran AOC"//color_reset

	do_p1 = .true.
	do_p2 = .true.
	if (args%part1 .and. .not. args%part2) do_p2 = .false.
	if (args%part2 .and. .not. args%part1) do_p1 = .false.

	p1 = ""
	p2 = ""

	if (do_p1) p1 = part1(args%input_filename)
	!if (do_p2) p2 = part2(args%input_filename)

	write(*,*) "    "//p1//":"//p2

	if (args%assert) then

		expect1 = "REPLACE_ME"
		expect2 = "REPLACE_ME"
		if (args%test) then
			expect1 = "REPLACE_ME"
			expect2 = "REPLACE_ME"
		end if

		if (do_p1 .and. p1 /= expect1) then
			write(*,*) ERROR_STR//"wrong part 1 answer"
			! Print expected value?
			error = .true.
		end if
		if (do_p2 .and. p2 /= expect2) then
			write(*,*) ERROR_STR//"wrong part 2 answer"
			error = .true.
		end if
		if (error) call panic("")
	end if
	call aoc_exit(EXIT_SUCCESS)

end program main

