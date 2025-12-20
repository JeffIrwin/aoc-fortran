
module aoc_m
	use iso_fortran_env
	use utils
	use map_m  ! TODO: add this to template but commented-out?
	implicit none
contains

!===============================================================================

function part1(filename) result(ans_)
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: ans_
	!********

	integer :: iu, io, sum_
	character(len = :), allocatable :: str_, source, sinks
	type(str_vec_t) :: chunks
	type(map_str_t) :: map

	map = new_map_str()
	sum_ = 0
	open(newunit = iu, file = filename, action = "read")
	do
		str_ = read_line(iu, io)
		if (io /= 0) exit
		print *, "str_ = ", str_

		chunks = split(str_, ":")
		source = chunks%vec(1)%str
		sinks  = chunks%vec(2)%str
		print *, "source = ", source
		print *, "sinks  = ", sinks

		call map%set(source, sinks)

		print *, ""
	end do
	close(iu)

	sum_ = search1(map, "you")

	write(*,*) "part 1 = ", sum_
	ans_ = to_str(sum_)

end function part1

!===============================================================================

recursive integer function search1(map, node) result(npaths)
	! Brute-force DFS, can revisit nodes
	type(map_str_t), intent(in) :: map
	character(len = *), intent(in) :: node
	!********
	character(len = :), allocatable :: sinks_str
	integer(kind=8) :: i
	type(str_vec_t) :: sinks

	print *, "search1 node '", node, "'"

	if (node == "out") then
		! Base case
		npaths = 1
		return
	end if

	sinks_str = map%get(node)
	print *, "sinks_str = ", sinks_str
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

	character(len = :), allocatable :: p1, p2, expect1, expect2, str
	logical :: do_p1, do_p2, error = .false., found
	type(args_t) :: args
	!type(map_i32_t) :: imap
	!type(map_str_t) :: map

	args = parse_args()

	do_p1 = .true.
	do_p2 = .true.
	if (args%part1 .and. .not. args%part2) do_p2 = .false.
	if (args%part2 .and. .not. args%part1) do_p1 = .false.

	p1 = ""
	p2 = ""
	write(*,*) fg_bright_magenta//"Starting Fortran AOC"//color_reset

	!****************
	! Test hash maps

	!str = "world"
	!print *, "Hash of '" // str // "' is: ", djb2_hash(str)

	!imap = new_map_i32()
	!call imap%set("foo", 1)
	!call imap%set("bar", 2)
	!call imap%set("baz", 3)
	!call imap%set(""  , 10)
	!call imap%set(" " , 11)
	!call imap%set("  ", 12)

	!print *, "imap['foo'] = ", imap%get("foo")
	!print *, "imap['bar'] = ", imap%get("bar")
	!print *, "imap['baz'] = ", imap%get("baz", found)
	!print *, "    (found = ", found, ")"
	!print *, "imap['barf'] = ", imap%get("barf", found)
	!print *, "    (found = ", found, ")"
	!print *, "imap[''] = ", imap%get("")
	!print *, "imap[' '] = ", imap%get(" ")
	!print *, "imap['  '] = ", imap%get("  ")

	!map = new_map_str()
	!call map%set("foo", "one")
	!call map%set("bar", "two")
	!call map%set("baz", "three")
	!print *, "map['foo'] = ", map%get("foo")
	!print *, "map['bar'] = ", map%get("bar")
	!print *, "map['baz'] = ", map%get("baz")

	!****************

	if (do_p1) p1 = part1(args%input_filename)
	!if (do_p2) p2 = part2(args%input_filename)

	write(*,*) "    "//p1//":"//p2

	if (args%assert) then

		expect1 = "643"
		expect2 = "417190406827152"
		if (args%test) then
			expect1 = "5"
			expect2 = "2"
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

