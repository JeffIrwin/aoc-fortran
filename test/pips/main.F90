
module aoc_m
	use iso_fortran_env
	use aoc_all_m
	implicit none

	!! Global variables
	!character, allocatable :: cg(:,:), rl(:), rt(:)
	!integer :: nx, ny, nr, nd
	!integer, allocatable :: rv(:)

	type pips_t
		! Pips game input data
		character, allocatable :: cg(:,:), rl(:), rt(:)
		integer :: nx, ny, nr, nd
		integer, allocatable :: rv(:)
		integer, allocatable :: ds(:,:)
	end type pips_t

	logical :: has_sln  ! shared mutable thread control. dangerous!

contains

!===============================================================================

function read_pips_text(filename) result(p)
	use blarg_m
	character(len=*), intent(in) :: filename
	type(pips_t) :: p
	!********
	character :: c
	character(len = :), allocatable :: str_
	integer :: i, iu, io, x, y

	! First pass: count the number of rows `ny`, regions `nr`, and dominoes `nd`
	p%ny = 0
	p%nr = 0
	p%nd = 0
	open(newunit = iu, file = filename, action = "read")
	i = 1
	do
		str_ = read_line(iu, io)
		if (io /= 0) exit
		print *, "str_ = ", str_
		!print *, "i = ", i

		if (str_ == "") then
			i = i + 1
			cycle
		end if

		if (i == 1) then
			p%ny = p%ny + 1
		else if (i == 2) then
			p%nr = p%nr + 1
		else if (i == 3) then
			p%nd = p%nd + 1
		else
			call panic("bad input -- too many blank lines")
		end if

	end do
	print *, "ny, nr, nd = ", p%ny, p%nr, p%nd
	rewind(iu)

	! Read the board
	str_ = read_line(iu, io)
	p%nx = len(str_)
	allocate(p%cg(p%nx, p%ny))
	p%cg = "."
	do y = 1, p%ny
		do x = 1, p%nx
			p%cg(x,y) = str_(x:x)
		end do

		str_ = read_line(iu, io)
	end do
	print *, "nx = ", p%nx
	call print_mat_char("cg = ", p%cg)

	! Read the region constraints, including labels `rl`, types `rt`, and values
	! `rv`
	allocate(p%rl(p%nr), p%rt(p%nr), p%rv(p%nr))
	p%rt = "."  ! default type: sum equal to given digit
	p%rv = -1
	do i = 1, p%nr
		str_ = read_line(iu, io)
		!print *, "str_ = ", str_

		p%rl(i) = str_(1:1)  ! label: a-z
		if (.not. any(p%cg == p%rl(i))) then
			call panic('constraint region "'//p%rl(i)//'" does not exist on input board')
		end if

		c = str_(4:4)
		if (is_digit(c)) then
			! Default type
			p%rv(i) = read_i32(str_(4:))

		else if (c == ">") then
			p%rt(i) = c
			p%rv(i) = read_i32(str_(5:))

		else if (c == "<") then
			p%rt(i) = c
			p%rv(i) = read_i32(str_(5:))

		else if (c == "=") then
			p%rt(i) = c  ! no value for this type
			if (str_(4:) /= "=") then
				call panic('end-of-line junk found: "'//str_(4:)//'"')
			end if

		else if (c == "!") then
			p%rt(i) = c
			if (str_(4:) /= "!=") then
				call panic('end-of-line junk found: "'//str_(4:)//'"')
			end if

		else
			call panic("bad input -- unexpected region constraint type")
		end if

	end do
	str_ = read_line(iu, io)  ! skip blank line

	print "(a,"//to_str(p%nr)//"a3)", " rl = ", p%rl
	print "(a,"//to_str(p%nr)//"a3)", " rt = ", p%rt
	print "(a,"//to_str(p%nr)//"i3)", " rv = ", p%rv

	! TODO: more input sanity checks:
	! - no region rules for "*" or ".", maybe even alphabetic only?
	! - no pip counts <0 or >6
	! - square input board, padding required
	! - split on delims to allow extra whitespace
	! - allow comments?

	! Sanity check on total area
	if (2 * p%nd < count(p%cg /= ".")) then
		call panic("too few dominoes to cover board")
	else if (2 * p%nd > count(p%cg /= ".")) then
		call panic("too many dominoes to fit in board")
	end if

	! Read the dominoes
	p%ds = zeros_i32(2, p%nd)
	do i = 1, p%nd
		str_ = read_line(iu, io)
		!print *, "str_ = ", str_
		p%ds(:,i) = read_i32_delims(str_, ", ")
	end do
	close(iu)
	call print_mat_i32("ds (transpose) = ", p%ds)

end function read_pips_text

!===============================================================================

function do_pips(args) result(ans_)
	use blarg_m
	type(args_t), intent(in) :: args
	character(len = :), allocatable :: ans_
	!********
	type(pips_t) :: p

	! TODO: select reader type based on filename extension, possibly json, then
	! solve all three difficulties
	p = read_pips_text(args%input_filename)
	ans_ = solve_pips(p)

end function do_pips

!===============================================================================

function solve_pips(p) result(ans_)
	type(pips_t), intent(in) :: p
	character(len = :), allocatable :: ans_
	!********
	character(len = :), allocatable :: ans1, ans2
	integer :: x, y
	integer, allocatable :: ig(:,:), idx1(:), idx2(:)
	integer, allocatable :: ds1(:,:), ds2(:,:)
	logical :: is_solvable1, is_solvable2
	logical, allocatable :: has_horz(:,:), has_vert(:,:)

	! Sort dominoes by the sum of each tile. This optimization makes the search
	! run >10x faster (from 1+ min on laptop battery for hard problem down to <2
	! sec)

	! Reverse search makes sense and is faster for 2025-12-27's hard problem
	!
	! Somehow forward search is faster for 2025-12-26's hard problem, and it is
	! much better on average and worst case
	!
	! Solve both sorts in parallel on different threads.  Most (?) problems
	! finish at least one thread quickly (<1s) and then the other thread could
	! stop.  Random unsorted permutations could be added in more threads but I
	! don't see the need unless there are problems that take longer
	idx1 = sort_index(sum(p%ds,1))
	idx2 = reverse(idx1)
	ds1 = p%ds(:, idx1)
	ds2 = p%ds(:, idx2)

	ig = -ones_i32(p%nx, p%ny)  ! initialize empty spots as -1
	has_horz = falses(p%nx, p%ny)
	has_vert = falses(p%nx, p%ny)
	do y = 1, p%ny
	do x = 1, p%nx
		if (p%cg(x,y) == ".") ig(x,y) = -2  ! mark the outside locations "." with -2
	end do
	end do
	!call print_mat_i32("ig (init) = ", transpose(ig))

	write(*,*) "Searching for solution ..."
	has_sln = .false.

!$omp parallel default(shared) private(ans_)
!$omp sections

!$omp section
	is_solvable1 = search(p, ds1, ig, 1, has_horz, has_vert, ans1)
	has_sln = has_sln .or. is_solvable1  ! careful re race conditions!
	!print *, "ans1 = ", ans1
	!print *, "section 1 done"

!$omp section
	is_solvable2 = search(p, ds2, ig, 1, has_horz, has_vert, ans2)
	has_sln = has_sln .or. is_solvable2
	!print *, "ans2 = ", ans2
	!print *, "section 2 done"

!$omp end sections nowait
!$omp end parallel

	if (is_solvable1) ans_ = ans1
	if (is_solvable2) ans_ = ans2

	if (.not. (is_solvable1 .or. is_solvable2)) then
		call panic("puzzle is not solvable")
	end if

end function solve_pips

!===============================================================================

recursive logical function search(p, ds, ig, id, has_horz, has_vert, sln) result(ans)
	! Pack the domino `id` into integer grid `ig`, return false if it violates
	! geometric or numeric constraints. Solution string `sln` is returned as
	! out-arg

	use utils_m  ! should be unnecessary but linter is mad
	type(pips_t), intent(in) :: p
	integer, intent(in) :: ds(:,:)
	integer, intent(inout) :: ig(:,:)
	integer, intent(in) :: id
	logical, intent(in) :: has_horz(:,:), has_vert(:,:)
	character(len=:), allocatable :: sln
	!********
	character :: c
	character, allocatable :: g(:,:)
	integer :: i, x0, y0, t, ndx, ndy, x, y, ic
	integer :: sums(128), vals(32, 128), nvals(128), sums_max(128)
	integer, allocatable :: d(:,:), igl(:,:)
	logical :: can_pack = .true.
	logical :: is_complete(128)  ! keys are ascii so arrays are size 128
	logical, allocatable :: has_horzl(:,:), has_vertl(:,:)

	if (has_sln) then
		sln = ""
		ans = .false.
		return
	end if

	if (id > size(ds,2)) then
!$omp critical
		ans = .true.  ! base case: all dominoes have been packed

		! Could also add an `idg` arg to show the domino ID that each solution
		! square came from

		! Double size to also print horizontal/vertical domino connections
		allocate(g(2*p%nx, 2*p%ny))
		g = " "
		do y = 1, p%ny
		do x = 1, p%nx
			if (ig(x,y) >= 0) g(2*x, 2*y) = to_str(ig(x,y))
			if (has_horz(x,y)) g(2*x+1, 2*y) = "-"
			if (has_vert(x,y)) g(2*x, 2*y+1) = "|"
		end do
		end do

		! Maybe this should return the char mat `g` instead of the single string `sln`
		call print_mat_char("answer = ", g)
		sln = mat_char_to_str(g, ":")
		print *, "sln = ", sln
!$omp end critical
		return
	end if

	! Place the current domino `id` in every possible position and orientation
	! (transformation)
	ans = .false.
	do y0 = 1, p%ny
	do x0 = 1, p%nx
	do t = 1, 4
		d = trans_(ds(:,id), t)
		ndx = size(d,1)
		ndy = size(d,2)
		!print *, "x0, y0, size(d) = ", x0, y0, ndx, ndy

		! Check bounds
		if (x0 + ndx > p%nx+1) cycle
		if (y0 + ndy > p%ny+1) cycle

		! Check if domino position is unoccupied
		can_pack = all(ig(x0: x0+ndx-1, y0: y0+ndy-1) == -1) ! TODO: magic numbers/chars
		if (.not. can_pack) cycle

		igl = ig  ! local copy
		igl(x0: x0+ndx-1, y0: y0+ndy-1) = d

		has_horzl = has_horz
		has_vertl = has_vert
		if (ndx > 1) then
			has_horzl(x0,y0) = .true.
		else
			has_vertl(x0,y0) = .true.
		end if

		! Check if the sums of each region satisfy the numeric constraints
		sums = 0
		sums_max = 0
		is_complete = .true.
		nvals = 0
		do y = 1, p%ny
		do x = 1, p%nx
			c = p%cg(x,y)
			if (c == "*") cycle  ! wildcard, free square
			ic = ichar(c)
			if (igl(x,y) < 0) then
				sums_max(ic) = sums_max(ic) + 6  ! max possible sum if all remaining squares are 6
				is_complete(ic) = .false.
				cycle
			end if
			sums(ic) = sums(ic) + igl(x,y)
			sums_max(ic) = sums_max(ic) + igl(x,y)
			nvals(ic) = nvals(ic) + 1
			vals(nvals(ic), ic) = igl(x,y)

		end do
		end do

		can_pack = .true.
		!print *, "sums = "
		do i = 1, p%nr
			ic = ichar(p%rl(i))
			!print *, rl(i), ": ", to_str(sums(ic))
			!print *, "rt = ", p%rt(i)
			!print *, "is_complete = ", is_complete(ic)

			select case (p%rt(i))
			case (".")
				if (is_complete(ic)) then
					can_pack = sums(ic) == p%rv(i)
				else
					can_pack = sums(ic) <= p%rv(i) .and. sums_max(ic) >= p%rv(i)
				end if
			case (">")
				if (is_complete(ic)) then
					can_pack = sums(ic) > p%rv(i)
				else
					can_pack = sums_max(ic) > p%rv(i)
				end if
			case ("<")
				can_pack = sums(ic) < p%rv(i)
			case ("=")
				can_pack = all_eq(vals(1: nvals(ic), ic))
			case ("!")
				can_pack = all_ne(vals(1: nvals(ic), ic))
			case default
				call panic("bad constraint type")
			end select
			if (.not. can_pack) exit
		end do
		if (.not. can_pack) cycle
		!call print_mat_i32("igl (wip) = ", transpose(igl))

		if (search(p, ds, igl, id+1, has_horzl, has_vertl, sln)) then
			ans = .true.
			return
		end if
	end do
	end do
	end do

end function search

!===============================================================================

logical function all_ne(v)
	! O(n**2), ok for small v
	integer, intent(in) :: v(:)
	integer :: i, j
	!print *, "all_ne, v = ", v
	all_ne = .true.
	do i = 2, size(v)
	do j = 1, i-1
		if (v(i) == v(j)) then
			all_ne = .false.
			return
		end if
	end do
	end do
end function all_ne

!===============================================================================

logical function all_eq(v)
	! O(n)
	integer, intent(in) :: v(:)
	!print *, "all_eq, v = ", v
	all_eq = .true.
	if (size(v) <= 1) return
	all_eq = all(v(2:) == v(1))
end function all_eq

!===============================================================================

function trans_(d, t) result(dt)
	! Make 1 of 4 possible transformations of a domino. Result is a 1x2 or 2x1
	! matrix
	integer, intent(in) :: d(2)
	integer, intent(in) :: t
	integer, allocatable :: dt(:,:)

	select case (t)
	case (1)
		allocate(dt(1,2))
		dt(1,:) = d

	case (2)
		allocate(dt(1,2))
		dt(1,:) = d([2,1])

	case (3)
		allocate(dt(2,1))
		dt(:,1) = d

	case (4)
		allocate(dt(2,1))
		dt(:,1) = d([2,1])

	case default
		call panic("bad transformation")
	end select

end function trans_

!===============================================================================

end module aoc_m

!===============================================================================

program main
	use aoc_m
	use args_m
	use utils_m
	implicit none

	character(len = :), allocatable :: p1, expect1
	logical :: error = .false.
	type(args_t) :: args

	args = parse_args()
	write(*,*) fg_bright_magenta//"Starting Fortran AOC"//color_reset

	p1 = do_pips(args)
	p1 = rm_char(p1, " ")
	print *, "p1 = ", p1

	select case (args%input_filename)
	case ("inputs/easy-2025-12-28.txt")
		expect1 = ":5-6::6:|:3::5-33:|:2-24:"

	case ("inputs/medium-2025-12-27.txt")
		expect1 = ":3-01-3::5-516-542-0:||:21:"
	case ("inputs/medium-2025-12-28.txt")
		expect1 = ":6:|:1-122-2::0-30-2::1-33-2:"

	case ("inputs/hard-2025-12-26.txt")
		expect1 = ":5-51-2::6-33-2::5-3::1-3::13:||:43::53-00:||:11-00::6-6::0:|:6::6-4:"
	case ("inputs/hard-2025-12-27.txt")
		expect1 = ":65:||:25::3333:||||:6512::6-02-5::4-04-1::52:||:11::::0:|:00:|:42:|:4:"
	case ("inputs/hard-2025-12-28.txt")
		expect1 = ":54-51:||:66-11::45:||:02::0-56-2::044-4:||:22::2-2:"

	case default
		expect1 = "REPLACE_ME"
	end select

	if (args%assert) then

		if (p1 /= expect1) then
			write(*,*) ERROR_STR//'wrong part 1 answer.  Got "' &
				//p1//'", expected "'//expect1//'"'
			error = .true.
		end if
		if (error) call panic("")
	end if
	call aoc_exit(EXIT_SUCCESS)

end program main

