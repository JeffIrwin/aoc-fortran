
module aoc_m
	use iso_fortran_env
	use aoc_all_m
	implicit none

	! Global variables. TODO: package these immutable vars into a pips_t game
	! struct
	character, allocatable :: cg(:,:), rl(:), rt(:)
	integer :: nx, ny, nr, nd
	integer, allocatable :: rv(:), ds(:,:)
	integer, allocatable :: pts(:,:)

contains

!===============================================================================

function part1(args) result(ans_)
	use blarg_m
	type(args_t), intent(in) :: args
	character(len = :), allocatable :: ans_
	!********
	character :: c
	character(len = :), allocatable :: filename, str_
	integer :: i, iu, io, x, y, np
	integer, allocatable :: ig(:,:), navail(:)
	integer(kind=8) :: sum_
	logical :: is_solvable
	logical, allocatable :: has_horz(:,:), has_vert(:,:)

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

	! Sort dominos by the sum of each tile. This optimization makes the search
	! run >10x faster (from 1+ min on laptop battery for hard problem down to <2
	! sec)

	! Reverse search makes sense and is faster for 2025-12-27's hard problem
	!
	! Somehow forward search is faster for 2025-12-26's hard problem, and it is
	! much better on average and worst case
	!
	! Could take both sorts, plus some random shuffles, and solve all in
	! parallel. Would need an atomic to print answer once and maybe caching

	!ds = ds(:, reverse(sort_index(sum(ds,1))))
	ds = ds(:, sort_index(sum(ds,1)))

	call print_mat_i32("ds (transpose) = ", ds)

	ig = -ones_i32(nx, ny)  ! initialize -1
	has_horz = falses(nx,ny)
	has_vert = falses(nx,ny)
	do y = 1, ny
	do x = 1, nx
		if (cg(x,y) == ".") ig(x,y) = -2  ! mark the outside locations "." with -2
	end do
	end do
	!call print_mat_i32("ig (init) = ", transpose(ig))

	! A lot (2x?) of work is wasted re-scanning over geometrically unpackable
	! positions. Make a pre-computed table before recursion of x, y, and t
	! values where dominoes can be packed, if sorting isn't enough optimization
	np = 0
	allocate(pts(3, 4*count(cg /= ".")))
	do y = 1, ny
	do x = 1, nx
		if (cg(x,y) == ".") cycle

		c = "."
		if (x < nx) c = cg(x+1, y)
		if (c /= ".") then
			np = np + 1
			pts(:,np) = [x, y, 3]
			np = np + 1
			pts(:,np) = [x, y, 4]
		end if

		c = "."
		if (y < ny) c = cg(x, y+1)
		if (c /= ".") then
			np = np + 1
			pts(:,np) = [x, y, 1]
			np = np + 1
			pts(:,np) = [x, y, 2]
		end if
	end do
	end do
	pts = pts(:, 1: np)  ! trim
	!print *, "pts = ", pts

	! Count the number of available squares of each pip count (0:6)
	navail = zeros_i32(7)
	do i = 1, nd
		navail(ds(1,i)+1) = navail(ds(1,i)+1) + 1
		navail(ds(2,i)+1) = navail(ds(2,i)+1) + 1
	end do
	!print *, "navail = ", navail

	write(*,*) "Searching for solution ..."
	is_solvable = search(ig, 1, has_horz, has_vert, navail, ans_)
	if (.not. is_solvable) then
		call panic("puzzle is not solvable")
	end if

end function part1

!===============================================================================

recursive logical function search(ig, id, has_horz, has_vert, navail, sln) result(ans)
	! Pack the domino `id` into integer grid `ig`, return false if it violates
	! geometric or numeric constraints. Solution string `sln` is returned as
	! out-arg

	use utils_m  ! should be unnecessary but linter is mad
	integer  , intent(inout) :: ig(:,:)
	integer  , intent(in) :: id
	logical, intent(in) :: has_horz(:,:), has_vert(:,:)
	integer, intent(in) :: navail(:)
	character(len=:), allocatable :: sln
	!********
	character :: c
	character, allocatable :: g(:,:)
	integer :: i, x0, y0, t, ndx, ndy, x, y, ic, ip, max_avail
	integer :: sums(128), vals(8, 128), nvals(128), sums_max(128)
	integer, allocatable :: d(:,:), igl(:,:), navaill(:)
	logical :: can_pack = .true.
	logical :: is_complete(128)  ! keys are ascii so arrays are size 128
	logical, allocatable :: has_horzl(:,:), has_vertl(:,:)

	if (id > size(ds,2)) then
		ans = .true.  ! base case: all dominoes have been packed

		! Could also add an `idg` arg to show the domino ID that each solution
		! square came from

		!call print_mat_i32("ig (ans) = ", transpose(ig))
		!call print_mat_bool("has_horz = ", transpose(has_horz))
		!call print_mat_bool("has_vert = ", transpose(has_vert))

		! Double size to also print horizontal/vertical domino connections
		allocate(g(2*nx, 2*ny))
		g = " "
		do y = 1, ny
		do x = 1, nx
			if (ig(x,y) >= 0) g(2*x, 2*y) = to_str(ig(x,y))
			if (has_horz(x,y)) g(2*x+1, 2*y) = "-"
			if (has_vert(x,y)) g(2*x, 2*y+1) = "|"
		end do
		end do

		! Maybe this should return the char mat `g` instead of the single string `sln`
		call print_mat_char("answer = ", g)
		sln = mat_char_to_str(g, ":")
		print *, "sln = ", sln
		return
	end if

	! Place the current domino `id` in every possible position and orientation
	! (transformation)
	ans = .false.
	do ip = 1, size(pts,2)
		x0 = pts(1,ip)  ! unpack
		y0 = pts(2,ip)
		t  = pts(3,ip)

		d = trans_(ds(:,id), t)
		ndx = size(d,1)
		ndy = size(d,2)
		!print *, "x0, y0, size(d) = ", x0, y0, ndx, ndy

		! Check if domino position is unoccupied
		can_pack = all(ig(x0: x0+ndx-1, y0: y0+ndy-1) == -1) ! TODO: magic numbers/chars
		if (.not. can_pack) cycle

		igl = ig  ! local copy
		igl(x0: x0+ndx-1, y0: y0+ndy-1) = d

		has_horzl = has_horz
		has_vertl = has_vert
		navaill = navail
		if (ndx > 1) then
			has_horzl(x0,y0) = .true.
		else
			has_vertl(x0,y0) = .true.
		end if

		! Decrement the available count
		navaill(ds(1,id)+1) = navaill(ds(1,id)+1) - 1
		navaill(ds(2,id)+1) = navaill(ds(2,id)+1) - 1
		max_avail = 0
		do i = 0, 6
			if (navaill(i+1) > 0) max_avail = i
		end do

		!! Almost always 6. Might not be worth it
		!if (max_avail < 6) print *, "max_avail = ", max_avail

		! Check if the sums of each region satisfy the numeric constraints
		sums = 0
		sums_max = 0
		is_complete = .true.
		nvals = 0
		do y = 1, ny
		do x = 1, nx
			c = cg(x,y)
			if (c == "*") cycle  ! wildcard, free square
			ic = ichar(c)
			if (igl(x,y) < 0) then
				!sums_max(ic) = sums_max(ic) + 6  ! max possible sum if all remaining squares are 6
				sums_max(ic) = sums_max(ic) + max_avail
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
					can_pack = sums(ic) <= rv(i) .and. sums_max(ic) >= rv(i)
				end if
			case (">")
				if (is_complete(ic)) then
					can_pack = sums(ic) > rv(i)
				else
					can_pack = sums_max(ic) > rv(i)
				end if
			case ("<")
				can_pack = sums(ic) < rv(i)
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

		if (search(igl, id+1, has_horzl, has_vertl, navaill, sln)) then
			ans = .true.
			return
		end if
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

	p1 = part1(args)
	p1 = rm_char(p1, " ")
	print *, "p1 = ", p1

	select case (args%input_filename)
	case ("easy-2025-12-28.txt")
		expect1 = ":5-6::6:|:3::5-33:|:2-24:"

	case ("medium-2025-12-27.txt")
		expect1 = ":3-01-3::5-516-542-0:||:21:"
	case ("medium-2025-12-28.txt")
		expect1 = ":6:|:1-122-2::0-30-2::1-33-2:"

	case ("hard-2025-12-26.txt")
		expect1 = ":5-51-2::6-33-2::5-3::1-3::13:||:43::53-00:||:11-00::6-6::0:|:6::6-4:"
	case ("hard-2025-12-27.txt")
		expect1 = ":65:||:25::3333:||||:6512::6-02-5::4-04-1::52:||:11::::0:|:00:|:42:|:4:"
	case ("hard-2025-12-28.txt")
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

