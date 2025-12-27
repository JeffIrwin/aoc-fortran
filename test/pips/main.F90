
module aoc_m
	use iso_fortran_env
	use aoc_all_m
	implicit none

	! Global variables
	character, allocatable :: cg(:,:), rl(:), rt(:)
	integer :: nx, ny, nr, nd
	integer, allocatable :: rv(:), ds(:,:)

contains

!===============================================================================

function part1(args) result(ans_)
	type(args_t), intent(in) :: args
	character(len = :), allocatable :: ans_
	!********
	character :: c
	character(len = :), allocatable :: filename, str_
	integer :: i, iu, io, x, y
	integer, allocatable :: ig(:,:)
	integer(kind=8) :: sum_
	logical :: is_solvable

	sum_ = 0

	! First pass: count the number of rows `ny`, regions `nr`, and dominoes `nd`
	ny = 0
	nr = 0
	nd = 0
	filename = args%input_filename
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
			ny = ny + 1
		else if (i == 2) then
			nr = nr + 1
		else if (i == 3) then
			nd = nd + 1
		else
			call panic("bad input -- too many blank lines")
		end if

	end do
	print *, "ny, nr, nd = ", ny, nr, nd
	rewind(iu)

	! Read the board
	str_ = read_line(iu, io)
	nx = len(str_)
	allocate(cg(nx, ny))
	cg = "."
	do y = 1, ny
		do x = 1, nx
			cg(x,y) = str_(x:x)
		end do

		str_ = read_line(iu, io)
	end do
	print *, "nx = ", nx
	call print_mat_char("cg = ", cg)

	! Read the region constraints, including labels `rl`, types `rt`, and values
	! `rv`
	allocate(rl(nr), rt(nr), rv(nr))
	rt = "."  ! default type: sum equal to given digit
	rv = -1
	do i = 1, nr
		str_ = read_line(iu, io)
		!print *, "str_ = ", str_

		rl(i) = str_(1:1)  ! label: a-z
		if (.not. any(cg == rl(i))) then
			call panic('constraint region "'//rl(i)//'" does not exist on input board')
		end if

		c = str_(4:4)
		if (is_digit(c)) then
			! Default type
			rv(i) = read_i32(str_(4:))

		else if (c == ">") then
			rt(i) = c
			rv(i) = read_i32(str_(5:))

		else if (c == "<") then
			rt(i) = c
			rv(i) = read_i32(str_(5:))

		else if (c == "=") then
			rt(i) = c  ! no value for this type
			if (str_(4:) /= "=") then
				call panic('end-of-line junk found: "'//str_(4:)//'"')
			end if

		else if (c == "!") then
			rt(i) = c
			if (str_(4:) /= "!=") then
				call panic('end-of-line junk found: "'//str_(4:)//'"')
			end if

		else
			call panic("bad input -- unexpected region constraint type")
		end if

	end do
	str_ = read_line(iu, io)  ! skip blank line

	print "(a,"//to_str(nr)//"a3)", " rl = ", rl
	print "(a,"//to_str(nr)//"a3)", " rt = ", rt
	print "(a,"//to_str(nr)//"i3)", " rv = ", rv

	! Sanity check on total area

	!print *, "2 * nd = ", 2 * nd
	!print *, "nboard = ", count(cg /= ".")
	if (2 * nd < count(cg /= ".")) then
		call panic("too few dominoes to cover board")
	else if (2 * nd > count(cg /= ".")) then
		call panic("too many dominoes to fit in board")
	end if

	! Read the dominoes
	ds = zeros_i32(2, nd)
	do i = 1, nd
		str_ = read_line(iu, io)
		!print *, "str_ = ", str_
		ds(:,i) = read_i32_delims(str_, ", ")
	end do
	close(iu)
	call print_mat_i32("ds (transpose) = ", ds)

	ig = -ones_i32(nx, ny)  ! initialize -1
	do y = 1, ny
	do x = 1, nx
		if (cg(x,y) == ".") ig(x,y) = -2  ! mark the outside locations "." with -2
	end do
	end do
	call print_mat_i32("ig (init) = ", transpose(ig))

	is_solvable = search(ig, 1)
	if (.not. is_solvable) then
		call panic("puzzle is not solvable")
	end if

	write(*,*) "part 1 = ", to_str(sum_)
	ans_ = to_str(sum_)

end function part1

!===============================================================================

recursive logical function search(ig, id) result(ans)
	! Pack the domino `id` into integer grid `ig`, return false if it violates
	! geometric or numeric constraints
	integer  , intent(inout) :: ig(:,:)
	integer  , intent(in) :: id
	!********
	character :: c
	integer :: i, x0, y0, t, ndx, ndy, x, y, ic
	integer :: sums(256), vals(8, 256), nvals(256)
	integer, allocatable :: d(:,:), igl(:,:)
	logical :: can_pack = .true.
	logical :: is_complete(256)

	if (id > size(ds,2)) then
		ans = .true.  ! base case: all dominoes have been packed
		call print_mat_i32("ig (ans) = ", transpose(ig))
		return
	end if

	ans = .false.
	nx = size(cg, 1)
	ny = size(cg, 2)

	! Place the current domino `id` in every possible position and orientation
	! (transformation)
	do y0 = 1, ny
	do x0 = 1, nx
	do t = 1, 4
		d = trans_(ds(:,id), t)
		ndx = size(d,1)
		ndy = size(d,2)
		!print *, "x0, y0, size(d) = ", x0, y0, ndx, ndy

		! Check bounds
		if (x0 + ndx > nx+1) cycle
		if (y0 + ndy > ny+1) cycle

		! Check domino position is unoccupied
		can_pack = all(ig(x0: x0+ndx-1, y0: y0+ndy-1) == -1)
		if (.not. can_pack) cycle

		igl = ig  ! local copy
		igl(x0: x0+ndx-1, y0: y0+ndy-1) = d

		! Check if the sums of each region satisfy the numeric constraints
		sums = 0
		is_complete = .true.
		nvals = 0
		do y = 1, ny
		do x = 1, nx
			c = cg(x,y)
			if (c == "*") cycle  ! wildcard, free square
			ic = ichar(c)
			if (igl(x,y) < 0) then
				is_complete(ic) = .false.
				cycle
			end if
			sums(ic) = sums(ic) + igl(x,y)
			nvals(ic) = nvals(ic) + 1
			vals(nvals(ic), ic) = igl(x,y)
		end do
		end do

		can_pack = .true.
		!print *, "sums = "
		do i = 1, nr
			ic = ichar(rl(i))
			!print *, rl(i), ": ", to_str(sums(ic))
			!print *, "rt = ", rt(i)
			!print *, "is_complete = ", is_complete(ic)

			select case (rt(i))
			case (".")
				if (is_complete(ic)) then
					can_pack = sums(ic) == rv(i)
				else
					! Could be more optimal in a few cases by analyzing sums and
					! max vals of unused dominoes
					can_pack = sums(ic) <= rv(i)
				end if
			case (">")
				if (is_complete(ic)) then
					can_pack = sums(ic) > rv(i)
				else
					can_pack = .true.
				end if
			case ("<")
				can_pack = sums(ic) < rv(i)
			case ("=")
				!can_pack = all(vals(2: nvals(ic), ic) == vals(1, ic))  ! wrong for nval <= 1
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

		if (search(igl, id+1)) then
			ans = .true.
			return
		end if
	end do
	end do
	end do

	! TODO: add `idg` arg to list which domino ID each final square came from or
	! just pretty-print the answer somehow in a way that shows whether dominos
	! are oriented vertically or horizontally
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

	if (do_p1) p1 = part1(args)
	!if (do_p2) p2 = part2(args)

	write(*,*) "    "//p1//":"//p2

	if (args%assert) then

		expect1 = "REPLACE_ME"
		expect2 = "REPLACE_ME"
		if (args%test) then
			expect1 = "REPLACE_ME"
			expect2 = "REPLACE_ME"
		end if

		if (do_p1 .and. p1 /= expect1) then
			write(*,*) ERROR_STR//'wrong part 1 answer.  Got "' &
				//p1//'", expected "'//expect1//'"'
			error = .true.
		end if
		if (do_p2 .and. p2 /= expect2) then
			write(*,*) ERROR_STR//'wrong part 2 answer.  Got "' &
				//p2//'", expected "'//expect2//'"'
			error = .true.
		end if
		if (error) call panic("")
	end if
	call aoc_exit(EXIT_SUCCESS)

end program main

