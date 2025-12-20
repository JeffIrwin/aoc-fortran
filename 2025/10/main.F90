
! The equivalent syntran program runs in 3 minutes.  This runs in 2 seconds
! compiled in the debug profile

module aoc_m
	use iso_fortran_env
	use utils
	implicit none
contains

!===============================================================================

function solve_ilp(a, b) result(iopt)
	use blarg_m
	integer, intent(in) :: a(:,:), b(:)
	integer, allocatable :: iopt(:)
	!********
	integer :: h, i, j, k, m, n, nfree, imax, i1(1), i0
	integer, allocatable :: inonz(:), ifree(:), imaxes(:), combos(:)
	logical :: is_valid
	real :: amax, f, fopt, sumx
	real, allocatable :: t(:,:), xopt(:), x(:)

	m = size(a,1)
	n = size(a,2)
	!print *, "m, n = ", m, n

	! Form augmented matrix tableau
	t = hstack(a, b)
	!call print_mat_f32("t ref = ", t)

	! Gaussian elimination to reduced row echelon form
	!
	! See part2.syntran or main.syntran for marginally more comments on the same
	! algorithm
	inonz = -ones_i32(m)
	ifree = zeros_i32(n)
	nfree = 0
	h = 1
	k = 1
	do while (h <= m .and. k <= n)
		! Find pivot
		i1 = maxloc(abs(t(h:m, k)))
		!print *, "i1 = ", i1
		imax = i1(1) + h - 1  ! almost easier to just scan manually, like below
		amax = -1.0
		if (imax >= 1) amax = abs(t(imax, k))
		!print *, "imax, amax = ", imax, amax

		if (amax < 0.0001) then
			nfree = nfree + 1
			ifree(nfree) = k
			k = k + 1
		else
			inonz(h) = k
			!print *, "swapping ", h, imax
			if (h /= imax) t([h, imax], :) = t([imax, h], :)

			do i = 1, m  ! RREF
				if (i == h) cycle
				f = t(i,k) / t(h,k)
				!print *, "f = ", f
				t(i,k) = 0.0
				t(i, k+1: n+1) = t(i, k+1: n+1) - t(h, k+1: n+1) * f
			end do

			h = h + 1
			k = k + 1
		end if
	end do
	!print *, "inonz = ", inonz
	!call print_mat_f32("t ref = ", t)

	! Get the rest of the free vars
	do k = m, n
		if (any(ifree == k)) cycle
		if (any(inonz == k)) cycle
		nfree = nfree + 1
		ifree(nfree) = k
	end do
	ifree = ifree(1: nfree)  ! trim
	!print *, "nfree = ", nfree
	!print *, "ifree = ", ifree

	imaxes = zeros_i32(n)
	do j = 1, n
	do i = 1, m
		if (a(i,j) /= 0) then
			imaxes(j) = max(imaxes(j), b(i))
		end if
	end do
	end do
	!print *, "imaxes (full) = ", imaxes

	xopt = zeros_f32(n)
	fopt = huge(fopt)
	!print *, "fopt = ", fopt

	! Iterate over possible value combinations of free vars
	imaxes = imaxes(ifree)
	combos = zeros_i32(nfree)
	!print *, "imaxes = ", imaxes

	i0 = -1
	do i = m, 1, -1
		if (inonz(i) >= 1) then
			i0 = i
			exit
		end if
	end do
	!print *, "i0 = ", i0

	x = zeros_f32(n)
	do
		sumx = sum(1.0 * combos)
		!print *, "combos = ", combos
		if (sumx < fopt) then
			is_valid = .true.
			x = 0.0
			x(ifree) = combos

			! Back substitute to solve for the other vars
			do i = i0, 1, -1
				k = inonz(i)
				!print *, "k = ", k
				x(k) = t(i, n+1)
				x(k) = x(k) - dot_product(t(i, ifree), x(ifree))
				x(k) = x(k) / t(i,k)

				is_valid = x(k) > -0.0001
				if (.not. is_valid) exit

				sumx = sumx + x(k)
				is_valid = sumx < fopt
				if (.not. is_valid) exit

				is_valid = is_int(x(k))
				if (.not. is_valid) exit
			end do

			if (is_valid) then
				fopt = sumx
				!print *, "fopt = ", fopt
				xopt = x
			end if
		end if
		if (.not. next_combo(combos, imaxes)) exit
	end do
	iopt = nint(xopt)

end function solve_ilp

!===============================================================================

logical function is_int(x)
	! TODO: utils
	real, intent(in) :: x
	is_int = abs(x - nint(x)) < 0.0001
end function is_int

!===============================================================================

logical function next_combo(c, n)
	! Bignum += 1 algo for number in array c with mixed radix
	integer, intent(inout) :: c(:)
	integer, intent(in) :: n(:)
	!********
	integer :: i, nc

	nc = size(c)
	next_combo = .false.
	if (nc == 0) return

	! Find first digit less than n-1
	i = 1
	do while (c(i) == n(i)-1)
		c(i) = 0
		i = i + 1
		if (i > nc) return
	end do
	c(i) = c(i) + 1
	next_combo = .true.

end function next_combo

!===============================================================================

function part2(filename) result(ans_)
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: ans_
	!********

	integer :: iu, io, sum_, i0, i1, num_jolts, num_buttons, n, ib
	integer, allocatable :: jolts_goal(:), ibuttons(:), buttons(:,:), iopt(:)
	character(len = :), allocatable :: str_, jolts_str, button_str

	sum_ = 0

	open(newunit = iu, file = filename, action = "read")
	do
		str_ = read_line(iu, io)
		!print *, "io = ", io
		if (io /= 0) exit
		!print *, "str_ = ", str_

		i0 = scan(str_, "{") + 1
		i1 = scan(str_, "}") - 1
		jolts_str = str_(i0: i1)

		num_jolts = count_str_match(jolts_str, ",") + 1
		allocate(jolts_goal(num_jolts))
		read(jolts_str, *) jolts_goal
		!print *, "jolts_goal = ", jolts_goal

		num_buttons = count_str_match(str_, "(")
		allocate(buttons(0:num_jolts-1, 0:num_buttons-1))
		buttons = 0

		! Parse buttons
		i0 = 0
		ib = 0
		do
			i0 = i0 + 1
			if (i0 > len(str_)) exit
			if (str_(i0:i0) /= "(") cycle

			i1 = i0 + 1
			do while (str_(i1:i1) /= ")")
				i1 = i1 + 1
			end do

			i0 = i0 + 1
			i1 = i1 - 1
			button_str = str_(i0: i1)

			n = count_str_match(button_str, ",") + 1
			allocate(ibuttons(n))
			read(button_str, *) ibuttons

			buttons(ibuttons, ib) = 1
			ib = ib + 1

			deallocate(ibuttons)
		end do
		!call print_mat_i32("buttons = ", buttons)

		iopt = solve_ilp(buttons, jolts_goal)
		sum_ = sum_ + sum(iopt)
		!print *, "Line: ", iline
		!print *, "Optimal solution = ", iopt
		!print *, "Optimal value    = ", sum(iopt)

		!********
		deallocate(jolts_goal)
		deallocate(buttons)
	end do
	close(iu)

	write(*,*) "part 2 = ", sum_
	ans_ = to_str(sum_)

end function part2

!===============================================================================

function part1(filename) result(ans_)
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: ans_
	!********

	integer :: i, iu, io, sum_, num_lights, num_buttons, n, ib, npress, &
		npress_min
	integer, allocatable :: ibuttons(:), combos(:), &
		imaxes(:)
	logical :: has_solution
	logical, allocatable :: lights_goal(:), state(:), buttons(:,:)
	character(len = :), allocatable :: str_, lights_str, button_str

	type(str_vec_t) :: words

	sum_ = 0

	open(newunit = iu, file = filename, action = "read")
	do
		str_ = read_line(iu, io)
		!print *, "io = ", io
		if (io /= 0) exit
		!print *, "str_ = ", str_

		words = split(str_, " ")
		lights_str = words%vec(1)%str
		!print *, "lights_str = ", lights_str
		num_lights = len(lights_str) - 2

		allocate(lights_goal(num_lights))
		lights_goal = .false.
		do i = 2, num_lights+1
			if (lights_str(i:i) == "#") lights_goal(i-1) = .true.
		end do
		!print *, "lights_goal = ", lights_goal

		num_buttons = int(words%len - 2)
		!print *, "num_buttons = ", num_buttons

		allocate(buttons(num_lights, num_buttons))
		buttons = .false.

		ib = 1
		do i = 2, num_buttons+1
			button_str = words%vec(i)%str
			!print *, "button_str = ", button_str

			! TODO: add parse_i32_delim() helper in utils
			n = count_str_match(button_str, ",") + 1
			allocate(ibuttons(n))
			read(button_str(2: len(button_str)-1), *) ibuttons
			!print *, "ibuttons = ", ibuttons

			buttons(ibuttons+1, ib) = .true.
			ib = ib + 1

			deallocate(ibuttons)
		end do
		!call print_mat_i32("buttons = ", buttons)

		!********
		! Solve the problem

		allocate(combos(num_buttons), imaxes(num_buttons))
		combos = 0
		imaxes = 2  ! press a button 0 or 1 times. twice is a no-op
		npress_min = huge(npress_min)
		do
			!print *, "combos = ", combos

			npress = sum(combos)
			if (allocated(state)) deallocate(state)
			allocate(state(num_lights))
			state = .false.
			do i = 1, num_buttons
				if (combos(i) == 0) cycle
				state = state .neqv. buttons(:,i)
			end do
			if (all(state .eqv. lights_goal)) then
				has_solution = .true.
				!print *, "npress = ", npress
				npress_min = min(npress_min, npress)
			end if

			if (.not. next_combo(combos, imaxes)) exit
		end do
		if (has_solution) then
			sum_ = sum_ + npress_min
		end if
		deallocate(combos, imaxes)

		!********
		deallocate(lights_goal)
		deallocate(buttons)
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

	do_p1 = .true.
	do_p2 = .true.
	if (args%part1 .and. .not. args%part2) do_p2 = .false.
	if (args%part2 .and. .not. args%part1) do_p1 = .false.

	p1 = ""
	p2 = ""
	write(*,*) fg_bright_magenta//"Starting Fortran AOC"//color_reset

	if (do_p1) p1 = part1(args%input_filename)
	if (do_p2) p2 = part2(args%input_filename)

	write(*,*) "    "//p1//":"//p2

	if (args%assert) then

		expect1 = "417"
		expect2 = "16765"
		if (args%test) then
			expect1 = "7"
			expect2 = "33"
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
	!write(*,*) "ending fortran main"
	call aoc_exit(EXIT_SUCCESS)

end program main

