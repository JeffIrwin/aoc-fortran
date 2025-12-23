
module aoc_m
	use iso_fortran_env
	use utils

	! TODO: add this `use map_m` to template but commented-out? Don't see a
	! downside to always including it. Actually just copy this back to the
	! template and down-edit it again. There are several other changes, e.g.
	! to_str(sum_) logging and TBD actual/expected assert logging, int kinds,
	! etc.
	use map_m
	use blarg_m

	implicit none
contains

!===============================================================================

function part1(filename) result(ans_)
	implicit none
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: ans_
	!********

	integer :: nx, ny, x, y
	integer(kind=8) :: sum_  ! TODO: fix kind in template
	character, allocatable :: g0(:,:), g(:,:)

	sum_ = 0
	g = read_mat_char(filename)
	!print *, "g = ", g
	!call print_mat_char("g = ", g)
	!call print_mat_char("g = ", g, transpose_ = .true.)

	g0 = g  ! backup unmodified grid to g0

	nx = size(g, 1)
	ny = size(g, 2)
	do y = 2, ny
		do x = 1, nx
			if (g0(x, y-1) == "S") g(x,y) = "|"

			if (g(x, y-1) == "|" .and. g0(x, y-1) /= "^") then
				g(x,y) = "|"
			end if
		end do

		do x = 1, nx
			if (x > 1) then
				if (g0(x-1, y) == "^" .and. g(x-1, y) == "|") then
					g(x,y) = "|"
					sum_ = sum_ + 1
				end if
			end if

			if (x < nx) then
				if (g0(x+1, y) == "^" .and. g(x+1, y) == "|") then
					g(x,y) = "|"
				end if
			end if
		end do
	end do
	!call print_mat_char("g = ", g)

	write(*,*) "part 1 = ", to_str(sum_)
	ans_ = to_str(sum_)

end function part1

!===============================================================================

function part2(filename) result(ans_)
	implicit none
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: ans_
	!********

	integer :: nx, ny, x, y
	integer(kind=8) :: sum_  ! TODO: fix kind in template
	character, allocatable :: g0(:,:), g(:,:)
	integer(kind=8), allocatable :: ig(:,:)

	sum_ = 0
	g = read_mat_char(filename)
	!print *, "g = ", g
	!call print_mat_char("g = ", g)
	g0 = g  ! backup unmodified grid to g0

	! This can be golfed down quite a bit more:
	!
	!     https://github.com/JeffIrwin/aoc-syntran/blob/main/2025/7/part-2-readable.syntran
	!
	! But this is the best I could quickly do without cheating

	! First pass: mark the locations that we can get to with pipe "|" characters
	nx = size(g, 1)
	ny = size(g, 2)
	do y = 2, ny
		do x = 1, nx
			if (g0(x, y-1) == "S") g(x,y) = "|"

			if (g(x, y-1) == "|" .and. g0(x, y-1) /= "^") then
				g(x,y) = "|"
			end if
		end do
		do x = 1, nx
			if (x > 1) then
				if (g0(x-1, y) == "^" .and. g(x-1, y) == "|") then
					g(x,y) = "|"
					sum_ = sum_ + 1
				end if
			end if
			if (x < nx) then
				if (g0(x+1, y) == "^" .and. g(x+1, y) == "|") then
					g(x,y) = "|"
				end if
			end if
		end do
	end do
	!call print_mat_char("g = ", g)

	! Second pass: add up path counts on integer matrix `ig`
	ig = zeros_i64(nx, ny)
	!call print_mat_i64("ig = ", ig)
	do y = 2, ny
		do x = 1, nx
			if (g0(x, y-1) == "S") ig(x,y) = 1

			if (g(x, y-1) == "|" .and. g0(x, y-1) /= "^") then
				ig(x,y) = ig(x,y) + ig(x, y-1)
			end if
		end do
		do x = 1, nx
			if (x > 1) then
				if (g0(x-1, y) == "^" .and. g(x-1, y) == "|") then
					ig(x,y) = ig(x,y) + ig(x-1, y)
				end if
			end if
			if (x < nx) then
				if (g0(x+1, y) == "^" .and. g(x+1, y) == "|") then
					ig(x,y) = ig(x,y) + ig(x+1, y)
				end if
			end if
		end do
	end do
	sum_ = sum(ig(:, ny))

	write(*,*) "part 2 = ", to_str(sum_)
	ans_ = to_str(sum_)

end function part2

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
	if (do_p2) p2 = part2(args%input_filename)

	write(*,*) "    "//p1//":"//p2

	if (args%assert) then

		expect1 = "1698"
		expect2 = "95408386769474"
		if (args%test) then
			expect1 = "21"
			expect2 = "40"
		end if

		if (do_p1 .and. p1 /= expect1) then
			write(*,*) ERROR_STR//"wrong part 1 answer"
			! TODO: print expected value
			error = .true.
		end if
		if (do_p2 .and. p2 /= expect2) then
			! TODO: this doesn't assert part 2 test correctly because it has a
			! different input and the "-t" arg is blocked
			write(*,*) ERROR_STR//"wrong part 2 answer"
			error = .true.
		end if
		if (error) call panic("")
	end if
	call aoc_exit(EXIT_SUCCESS)

end program main

