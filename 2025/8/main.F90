
module aoc_m
	use iso_fortran_env
	use args_m
	use blarg_m
	use map_m
	use sort_m  ! TODO: add to template, or better yet, make a container module in lib
	use utils
	implicit none
contains

!===============================================================================

function part1(args) result(ans_)
	type(args_t), intent(in) :: args
	character(len = :), allocatable :: ans_
	!********

	character(len = :), allocatable :: filename, str_
	integer, parameter :: NADJ_CAP = 16
	integer :: iu, io, n, i, j, k, i1, i2, ncon, icom, ncom
	integer, allocatable :: mat(:,:), adj(:,:), nadj(:), icoms(:), ncoms(:)
	integer(kind=8) :: sum_, diff(3)
	integer(kind=8), allocatable :: dists(:,:), idx(:)

	filename = args%input_filename

	n = count_lines(filename)
	allocate(mat(3,n))
	sum_ = 0
	open(newunit = iu, file = filename, action = "read")
	do i = 1, n
		str_ = read_line(iu, io)
		if (io /= 0) exit
		!print *, "str_ = ", str_

		! TODO: make read_mat_i32_delims() helper
		mat(:,i) = read_i32_delims(str_, ",")
		!print *, "v = ", v
	end do
	close(iu)
	!call print_mat_i32("mat = ", transpose(mat))

	! Get squared distance between every pair of nodes
	allocate(dists(3, n * (n-1) / 2))
	k = 0
	do i = 1, n
	do j = 1, i-1
		k = k + 1
		diff = mat(:,i) - mat(:,j)
		dists(:,k) = [dot_product(diff, diff), int(i,8), int(j,8)]
	end do
	end do
	!call print_mat_i64("dists = ", transpose(dists), 9)

	idx = sortidx_i64(dists(1,:))
	!print *, "idx = ", idx
	!call print_mat_i64("dists (sorted) = ", transpose(dists(:,idx)), 9)

	! Number of pairs to connect
	ncon = 1000
	if (args%test) ncon = 10
	print *, "ncon = ", ncon

	adj = zeros_i32(NADJ_CAP, n)
	nadj = zeros_i32(n)
	do i = 1, ncon
		i1 = dists(2, idx(i))
		i2 = dists(3, idx(i))

		!print *, "i12 = ", i1, i2

		! Connect node i1 to i2 and vice-versa
		nadj(i1) = nadj(i1) + 1
		nadj(i2) = nadj(i2) + 1
		adj(nadj(i1), i1) = i2
		adj(nadj(i2), i2) = i1

	end do
	!call print_mat_i32("adj = ", transpose(adj))

	! Mark the connected components in the graph
	icom = 1
	icoms = zeros_i32(n)
	do i = 1, n
		if (icoms(i) > 0) cycle

		! Node `i` is in component `icom`. Do a DFS and mark all the nodes that
		! are connected to it
		call dfs(i, icom, icoms, adj, nadj)

		icom = icom + 1
	end do
	!print *, "icoms = ", icoms
	!print *, "icom = ", icom
	!print *, "maxval(icoms) = ", maxval(icoms)
	ncom = icom - 1

	! Count the size of each component
	ncoms = zeros_i32(ncom)
	do i = 1, ncom
		ncoms(i) = count(icoms == i)
	end do
	!print *, "ncoms = ", ncoms
	idx = sortidx_i32(ncoms)
	ncoms = ncoms(idx)

	sum_ = product(ncoms(ncom-2: ncom))

	write(*,*) "part 1 = ", to_str(sum_)
	ans_ = to_str(sum_)

end function part1

!===============================================================================

function part2(args) result(ans_)
	type(args_t), intent(in) :: args
	character(len = :), allocatable :: ans_
	!********

	character(len = :), allocatable :: filename, str_
	integer, parameter :: NADJ_CAP = 64
	integer :: iu, io, n, i, j, k, i1, i2, ncon, icom, ncom
	integer, allocatable :: mat(:,:), adj(:,:), nadj(:), icoms(:), ncoms(:)
	integer(kind=8) :: sum_, diff(3)
	integer(kind=8), allocatable :: dists(:,:), idx(:)

	filename = args%input_filename

	n = count_lines(filename)
	allocate(mat(3,n))
	sum_ = 0
	open(newunit = iu, file = filename, action = "read")
	do i = 1, n
		str_ = read_line(iu, io)
		if (io /= 0) exit
		mat(:,i) = read_i32_delims(str_, ",")
	end do
	close(iu)
	!call print_mat_i32("mat = ", transpose(mat))

	! Get squared distance between every pair of nodes
	allocate(dists(3, n * (n-1) / 2))
	k = 0
	do i = 1, n
	do j = 1, i-1
		k = k + 1
		diff = mat(:,i) - mat(:,j)
		dists(:,k) = [dot_product(diff, diff), int(i,8), int(j,8)]
	end do
	end do
	!call print_mat_i64("dists = ", transpose(dists), 9)

	idx = sortidx_i64(dists(1,:))
	!print *, "idx = ", idx
	!call print_mat_i64("dists (sorted) = ", transpose(dists(:,idx)), 9)

	adj = zeros_i32(NADJ_CAP, n)
	nadj = zeros_i32(n)
	do j = 1, size(dists,2)
		i1 = dists(2, idx(j))
		i2 = dists(3, idx(j))

		!print *, "i12 = ", i1, i2

		! Connect node i1 to i2 and vice-versa
		nadj(i1) = nadj(i1) + 1
		nadj(i2) = nadj(i2) + 1
		adj(nadj(i1), i1) = i2
		adj(nadj(i2), i2) = i1

		! Mark the connected components in the graph
		icom = 1
		icoms = zeros_i32(n)
		do i = 1, n
			if (icoms(i) > 0) cycle
	
			! Node `i` is in component `icom`. Do a DFS and mark all the nodes that
			! are connected to it
			call dfs(i, icom, icoms, adj, nadj)
			if (all(icoms == 1)) then
				sum_ = mat(1,i1) * mat(1,i2)
				exit
			else
				exit
			end if
	
			!icom = icom + 1
		end do
		if (sum_ > 0) exit
	end do
	!call print_mat_i32("adj = ", transpose(adj))

	write(*,*) "part 2 = ", to_str(sum_)
	ans_ = to_str(sum_)

end function part2

!===============================================================================

recursive subroutine dfs(i, icom, icoms, adj, nadj)
	integer, intent(in) :: i, icom
	integer, intent(inout) :: icoms(:)
	integer, intent(in) :: adj(:,:), nadj(:)
	!********
	integer :: k

	if (icoms(i) > 0) return
	icoms(i) = icom
	do k = 1, nadj(i)
		call dfs(adj(k,i), icom, icoms, adj, nadj)
	end do

end subroutine dfs

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
	if (do_p2) p2 = part2(args)

	write(*,*) "    "//p1//":"//p2

	if (args%assert) then

		expect1 = "123420"
		expect2 = "673096646"
		if (args%test) then
			expect1 = "40"
			expect2 = "25272"
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

