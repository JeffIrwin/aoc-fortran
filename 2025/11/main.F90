
module aoc_m
	use iso_fortran_env
	use utils
	use blarg_m
	use map_m

	implicit none
contains

!===============================================================================

function part1(filename) result(ans_)
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: ans_
	!********

	integer :: iu, io
	integer(kind=8) :: sum_
	character(len = :), allocatable :: str_, source, sinks
	type(str_vec_t) :: chunks
	type(map_str_t) :: map

	map = new_map_str()
	sum_ = 0
	open(newunit = iu, file = filename, action = "read")
	do
		str_ = read_line(iu, io)
		if (io /= 0) exit
		!print *, "str_ = ", str_

		chunks = split(str_, ":")
		source = chunks%vec(1)%str
		sinks  = chunks%vec(2)%str
		!print *, "source = ", source
		!print *, "sinks  = ", sinks

		call map%set(source, sinks)
	end do
	close(iu)

	sum_ = search1(map, "you")

	write(*,*) "part 1 = ", to_str(sum_)
	ans_ = to_str(sum_)

end function part1

!===============================================================================

function part2(filename) result(ans_)
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: ans_
	!********

	integer :: iu, io
	integer(kind=8) :: sum_
	integer(kind=8) :: nsvr2fft, nfft2dac, ndac2out, nsvr2dac, ndac2fft, nfft2out
	character(len = :), allocatable :: str_, source, sinks
	type(str_vec_t) :: chunks
	type(map_str_t) :: map
	type(map_i64_t) :: cache

	map = new_map_str()
	sum_ = 0
	open(newunit = iu, file = filename, action = "read")
	do
		str_ = read_line(iu, io)
		if (io /= 0) exit

		chunks = split(str_, ":")
		source = chunks%vec(1)%str
		sinks  = chunks%vec(2)%str
		call map%set(source, sinks)
	end do
	close(iu)

	!****************
	!
	! Divide the problem into several smaller ones. Ignoring intermediate
	! nodes, there are 2 ways to get from "svr" to "out" with the desired nodes
	! in between:
	!
	!         -> dac -> fft
	!        /n1     n2    \n3
	!     svr               -> out
	!        \na     nb    /nc
	!         -> fft -> dac
	!
	! While counting the ways to get *directly* from svr to dac, we want to
	! ignore fft.  That is what the `avoid` argument in count_paths() is for
	!
	! Then the total number of paths from svr to out is:
	!
	!      n1*n2*n3 + na*nb*nc
	!
	! Where n1 is the number of paths from svr to dac, etc.
	!
	!****************

	cache = new_map_i64()
	nsvr2dac = count_paths(map, cache, "svr", "dac", "fft") ! avoid fft here
	!print *, "nsvr2dac = ", nsvr2dac

	cache = new_map_i64() ! cache invalidation is hard :(
	nsvr2fft = count_paths(map, cache, "svr", "fft", "dac") ! avoid fft here
	!****************

	cache = new_map_i64()
	ndac2fft = count_paths(map, cache, "dac", "fft", "")  ! nothing to avoid here

	cache = new_map_i64()
	nfft2dac = count_paths(map, cache, "fft", "dac", "")
	!****************

	cache = new_map_i64()
	ndac2out = count_paths(map, cache, "dac", "out", "fft")

	cache = new_map_i64()
	nfft2out = count_paths(map, cache, "fft", "out", "dac")
	!****************

	sum_ = &
		(nsvr2fft * nfft2dac * ndac2out) + &
		(nsvr2dac * ndac2fft * nfft2out)

	write(*,*) "part 2 = ", to_str(sum_)
	ans_ = to_str(sum_)

end function part2

!===============================================================================

recursive function count_paths(map, cache, node, dest, avoid) result(npaths)
	! Count the number of paths from `node` to `dest` without hitting the
	! `avoid` node.  The graph connectivity is defined in `map` and a `cache`
	! is used to help count efficiently without re-visiting sub-paths multiple
	! times
	type(map_str_t), intent(in) :: map
	type(map_i64_t), intent(inout) :: cache
	character(len=*), intent(in) :: node, dest, avoid
	integer(kind=8) :: npaths
	!********
	character(len=:), allocatable :: children_str
	integer(kind=8) :: i, hit
	logical :: found
	type(str_vec_t) :: children

	hit = cache%get(node, found)
	if (found) then
		! Cache hit
		npaths = hit
		return
	end if

	! Otherwise, cache miss

	! Base cases
	if (node == avoid) then
		call cache%set(node, 0_8)
		npaths = 0
		return
	end if
	if (node == dest) then
		call cache%set(node, 1_8)
		npaths = 1
		return
	end if

	! Recursive case -- sum up the children's path counts
	npaths = 0
	children_str = map%get(node)
	children = split(children_str, " ")
	do i = 1, children%len
		npaths = npaths + count_paths(map, cache, children%vec(i)%str, dest, avoid)
	end do
	call cache%set(node, npaths)

end function count_paths

!===============================================================================

recursive integer function search1(map, node) result(npaths)
	! Brute-force DFS, can revisit nodes
	type(map_str_t), intent(in) :: map
	character(len = *), intent(in) :: node
	!********
	character(len = :), allocatable :: sinks_str
	integer(kind=8) :: i
	type(str_vec_t) :: sinks

	!print *, "search1 node '", node, "'"

	if (node == "out") then
		! Base case
		npaths = 1
		return
	end if

	sinks_str = map%get(node)
	!print *, "sinks_str = ", sinks_str
	sinks = split(sinks_str, " ")
	!call print_str_vec("sinks = ", sinks)
	npaths = 0
	do i = 1, sinks%len
		npaths = npaths + search1(map, sinks%vec(i)%str)
	end do

end function search1

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

		expect1 = "643"
		expect2 = "417190406827152"
		if (args%test) then
			expect1 = "5"
			expect2 = "2"
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

