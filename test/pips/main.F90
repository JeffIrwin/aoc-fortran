
module aoc_m
	use iso_fortran_env
	use aoc_all_m
	use pips_io_m
	implicit none

	type mat_i32
		! Wrapper class so each domino can have a different sized list of valid positions
		integer, allocatable :: mat(:,:)
	end type mat_i32

contains

!===============================================================================

function do_pips(args) result(ans_)
	type(args_t), intent(in) :: args
	character(len = :), allocatable :: ans_
	!********
	character(len = :), allocatable :: d
	integer :: i
	type(pips_t) :: p
	type(str_vec_t) :: difficulties

	if (ends_with(args%input_filename, ".txt")) then
		p = read_pips_text(args%input_filename)
		ans_ = solve_pips(p)
	else if (ends_with(args%input_filename, ".json")) then

		difficulties = new_str_vec()
		call difficulties%push("easy")
		call difficulties%push("medium")
		call difficulties%push("hard")

		ans_ = ""
		do i = 1, i32(difficulties%len)
			d = difficulties%vec(i)%str
			p = read_pips_json(args%input_filename, d)
			call print_pips(p)
			ans_ = ans_ // solve_pips(p) // ";"
		end do

	else
		call panic('bad file extension in "'//args%input_filename//'"')
	end if

end function do_pips

!===============================================================================

subroutine print_pips(p)
	type(pips_t), intent(in) :: p

	write(*,*) "Number of dominoes = ", p%nd

	call print_mat_i32("dominoes = ", p%ds)
	write(*,*)

end subroutine print_pips

!===============================================================================

function solve_pips(p) result(ans_)
	type(pips_t), intent(in) :: p
	character(len = :), allocatable :: ans_
	!********
	integer :: x, y, x0, y0, t, id, i, nmatch, nnomatch
	integer, allocatable :: ig(:,:), ds(:,:), idx(:), d(:,:), igl(:,:), rn(:), &
		navail(:), imatch(:), inomatch(:)
	logical :: is_solvable, match
	logical, allocatable :: has_horz(:,:), has_vert(:,:)
	integer, allocatable :: np(:)
	type(mat_i32), allocatable :: pos(:)

	ig = -ones_i32(p%nx, p%ny)  ! initialize empty spots as -1
	has_horz = falses(p%nx, p%ny)
	has_vert = falses(p%nx, p%ny)
	do y = 1, p%ny
	do x = 1, p%nx
		if (p%cg(x,y) == ".") ig(x,y) = -2  ! mark the outside locations "." with -2
	end do
	end do
	!call print_mat_i32("ig (init) = ", transpose(ig))

	! Get the size of each region
	rn = zeros_i32(p%nr)
	do i = 1, p%nr
		rn(i) = count(p%cg == p%rl(i))
	end do
	print *, "rn = ", rn

	! Count the number of available squares of each pip count (0:6)
	navail = zeros_i32(7)
	do i = 1, p%nd
		navail(p%ds(1,i)+1) = navail(p%ds(1,i)+1) + 1
		navail(p%ds(2,i)+1) = navail(p%ds(2,i)+1) + 1
	end do
	print *, "navail = ", navail

	! Before starting recursive search, make a pass where we use is_valid() to
	! find all possible positions/orientations of each domino in isolation. A
	! single domino by itself in isolation can violate numeric constraints, so
	! this prunes down the search space
	!
	! I had something similar that I removed here:
	!
	!     https://github.com/JeffIrwin/aoc-fortran/commit/b2f07ccce84c746578ccdd0a683828cfcc48eb11
	!
	! That only checked geometric constraints.  Numeric constraints are more
	! complicated because each domino will have different possibilities

	allocate(pos( p%nd ))
	np = zeros_i32(p%nd)
	do id = 1, p%nd

		allocate(pos(id)%mat( 3, 4*count(p%cg /= ".") ))  ! over-allocate
		do y0 = 1, p%ny
		do x0 = 1, p%nx
		do t = 1, 4

			d = trans_(p%ds(:,id), t)
			igl = ig  ! local copy

			if (.not. is_valid(p, rn, igl, navail, d, x0, y0)) cycle
			np(id) = np(id) + 1
			pos(id)%mat(:, np(id)) = [x0, y0, t]

		end do
		end do
		end do

		! The pos matrices could be fragmented in memory. Might be better to
		! make one big matrix shared by all dominoes, along with a start/end
		! index for each domino
		pos(id)%mat = pos(id)%mat(:, 1: np(id))  ! trim
		!call print_mat_i32("pos "//to_str(id)//" = ", pos(id)%mat)

	end do

	! Sort dominoes by their number of valid positions. This optimization makes
	! the search run >>10x faster (from 1+ min on laptop battery for hard
	! problem down to <2 sec)

	! TODO: 2025-10-28 is hard, I think we still need special handling for
	! single-square sum constraints as this might prune down that particular
	! problem. Might want to go back to something like the `navail` technique:
	!
	!     https://github.com/JeffIrwin/aoc-fortran/commit/2c6d8d41f9a22fff841a82015c920fc84fd41bc0

	idx = sort_index(np)
	ds = p%ds(:, idx)
	pos = pos(idx)

	! Put any dominoes matching single-square sum constraints at the front
	! (end?) of the list
	nmatch = 0
	nnomatch = 0
	imatch   = zeros_i32(p%nd)
	inomatch = zeros_i32(p%nd)
	do id = 1, p%nd
		print *, "domino = ", ds(:,id)
		match = .false.
		do i = 1, p%nr

			if (p%rt(i) /= ".") cycle
			!if (.not. any(p%rt(i) == [".", "<"])) cycle  ! TODO: add ">" to this list when ready

			if (rn(i) /= 1) cycle

			! TODO: modify for < or > vs .
			match = any(ds(:,id) == p%rv(i))

			if (match) exit
		end do
		if (match) then
			print *, "match"
			nmatch = nmatch + 1
			imatch(nmatch) = id
		else
			nnomatch = nnomatch + 1
			inomatch(nnomatch) = id
		end if
	end do
	imatch = imatch(1: nmatch)  ! trim
	inomatch = inomatch(1: nnomatch)
	print *, "imatch = ", imatch
	print *, "inomatch = ", inomatch

	idx = [imatch, inomatch]
	!idx = [inomatch, imatch]
	ds = ds(:, idx)
	pos = pos(idx)

	call print_mat_i32("dominoes (sorted) = ", ds)
	print *, "np  (sorted) = ", np(idx)

	write(*,*) "Searching for solution ..."

	is_solvable = search(p, rn, ds, pos, ig, navail, 1, has_horz, has_vert, ans_)
	!print *, "section 1 done"

	if (.not. is_solvable) then
		call panic("puzzle is not solvable")
	end if

end function solve_pips

!===============================================================================

recursive logical function search(p, rn, ds, pos, ig, navail, id, has_horz, has_vert, sln) result(ans)
	! Pack the domino `id` into integer grid `ig`, return false if it violates
	! geometric or numeric constraints. Solution string `sln` is returned as
	! out-arg
	type(pips_t), intent(in) :: p
	integer, intent(in) :: rn(:)
	integer, intent(in) :: ds(:,:)
	type(mat_i32), intent(in) :: pos(:)
	integer, intent(inout) :: ig(:,:)
	integer, intent(in) :: navail(:)
	integer, intent(in) :: id
	logical, intent(in) :: has_horz(:,:), has_vert(:,:)
	character(len=:), allocatable :: sln
	!********
	character, allocatable :: g(:,:)
	integer :: x0, y0, t, x, y, ip
	integer, allocatable :: d(:,:), igl(:,:), navaill(:)
	logical, allocatable :: has_horzl(:,:), has_vertl(:,:)

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
	do ip = 1, size(pos(id)%mat,2)
		x0 = pos(id)%mat(1, ip)
		y0 = pos(id)%mat(2, ip)
		t  = pos(id)%mat(3, ip)

		d = trans_(ds(:,id), t)
		igl = ig  ! local copy
		if (.not. is_valid(p, rn, igl, navail, d, x0, y0)) cycle

		has_horzl = has_horz
		has_vertl = has_vert
		navaill = navail
		if (size(d,1) > 1) then
			has_horzl(x0,y0) = .true.
		else
			has_vertl(x0,y0) = .true.
		end if

		! Decrement the available count
		navaill(ds(1,id)+1) = navaill(ds(1,id)+1) - 1
		navaill(ds(2,id)+1) = navaill(ds(2,id)+1) - 1
		!max_avail = 0
		!do i = 0, 6
		!	if (navaill(i+1) > 0) max_avail = i
		!end do

		if (search(p, rn, ds, pos, igl, navaill, id+1, has_horzl, has_vertl, sln)) then
			ans = .true.
			return
		end if
	end do

end function search

!===============================================================================

logical function is_valid(p, rn, igl, navail, d, x0, y0)
	type(pips_t), intent(in) :: p
	integer, intent(in) :: rn(:)
	integer, intent(inout) :: igl(:,:)
	integer, intent(in) :: navail(:)
	integer, intent(in) :: d(:,:), x0, y0
	!********
	character :: c
	integer :: i, ndx, ndy, x, y, ic
	integer :: sums(128), vals(32, 128), nvals(128), sums_max(128)
	logical :: can_pack = .true.
	logical :: is_complete(128)  ! keys are ascii so arrays are size 128

	is_valid = .false.

	ndx = size(d,1)
	ndy = size(d,2)
	!print *, "x0, y0, size(d) = ", x0, y0, ndx, ndy

	! Check bounds
	if (x0 + ndx > p%nx+1) return
	if (y0 + ndy > p%ny+1) return

	! Check if domino position is unoccupied
	can_pack = all(igl(x0: x0+ndx-1, y0: y0+ndy-1) == -1) ! TODO: magic numbers/chars
	if (.not. can_pack) return

	igl(x0: x0+ndx-1, y0: y0+ndy-1) = d

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
			else if (rn(i) == 1) then
				! Handle special case for single-square sum constraints here
				can_pack = navail( p%rv(i) + 1 ) > 0
				!can_pack = navail( p%rv(i) ) > 0
				!print *, "can_pack = ", can_pack
				!if (.not. can_pack) stop
			else
				can_pack = sums(ic) <= p%rv(i) .and. sums_max(ic) >= p%rv(i)
			end if
		case (">")
			! TODO: handle special single-square cases for ">" and "<" too.
			! Careful with off-by-one indexing of navail(0:6)
			if (is_complete(ic)) then
				can_pack = sums(ic) > p%rv(i)
			else
				can_pack = sums_max(ic) > p%rv(i)
			end if
		case ("<")
			!if (rn(i) == 1) then
			if (.not. is_complete(ic) .and. rn(i) == 1) then
				!can_pack = sums(ic) < p%rv(i) .or. any(navail( 1: p%rv(i) ) > 0)
				can_pack = any(navail( 1: p%rv(i) ) > 0)
				!!if (is_complete(ic)
				!if (.not. can_pack) then
				!	print *, "can_pack = ", can_pack
				!	call exit(1)
				!end if
			else
				can_pack = sums(ic) < p%rv(i)
			end if
		case ("=")
			can_pack = all_eq(vals(1: nvals(ic), ic))
		case ("!")
			can_pack = all_ne(vals(1: nvals(ic), ic))
		case default
			call panic("bad constraint type")
		end select
		if (.not. can_pack) exit
	end do
	if (.not. can_pack) return
	!call print_mat_i32("igl (wip) = ", transpose(igl))

	is_valid = .true.

end function is_valid

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
	case ("inputs/easy-2025-12-30.txt")
		expect1 = ":4:|:4-24::3-33-43:|:1:"

	case ("inputs/medium-2025-12-27.txt")
		expect1 = ":3-01-3::5-516-542-0:||:21:"
	case ("inputs/medium-2025-12-28.txt")
		expect1 = ":6:|:1-122-2::0-30-2::1-33-2:"
	case ("inputs/medium-2025-12-30.txt")
		expect1 = ":3-53:|:6-161:|:4-46-21::0-0:"

	case ("inputs/hard-2025-12-26.txt")
		expect1 = ":5-51-2::6-33-2::5-3::1-3::13:||:43::53-00:||:11-00::6-6::0:|:6::6-4:"
	case ("inputs/hard-2025-12-27.txt")
		expect1 = ":65:||:25::3333:||||:6512::6-02-5::4-04-1::52:||:11::::0:|:00:|:42:|:4:"
	case ("inputs/hard-2025-12-28.txt")
		expect1 = ":54-51:||:66-11::45:||:02::0-56-2::044-4:||:22::2-2:"
	case ("inputs/hard-2025-12-30.txt")
		expect1 = ":6-55-5::6455:||||:6013::633-3:||:42::02:||:02::4-2:"

	case ("inputs/2025-12-24.json")
		expect1 =  ":0:|:2::24-6:|:66:|:1-33:;:1-30-4::011:|||:040::5-53-4:;:3-36-4::6444:||||:5203::2-33-0::1-5::10:||:25::1-6:;"

	case default
		expect1 = "REPLACE_ME"
	end select

	if (args%assert) then

		if (p1 /= expect1) then
			write(*,*) ERROR_STR//'wrong answer.  Got "' &
				//p1//'", expected "'//expect1//'"'
			error = .true.
		end if
		if (error) call panic("")
	end if
	call aoc_exit(EXIT_SUCCESS)

end program main

