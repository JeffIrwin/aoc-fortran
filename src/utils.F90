
module utils_m

	use iso_fortran_env
	implicit none

	double precision, parameter :: PI = 4.d0 * atan(1.d0)

	integer, parameter :: &
		EXIT_FAILURE = -1, &
		EXIT_SUCCESS = 0

	character, parameter :: &
			NULL_CHAR       = char( 0), &
			TAB             = char( 9), &
			LINE_FEED       = char(10), &
			VERT_TAB        = char(11), &
			CARRIAGE_RETURN = char(13), &
			ESC             = char(27)

	! TODO: make these variables, with colors disabled if output_unit is not tty
	! and an option to --force-color
	character(len = *), parameter :: &
			fg_bold               = ESC//"[;1m", &
			fg_yellow             = ESC//"[33m", &
			fg_bold_yellow        = ESC//"[33;1m", &
			fg_bright_red         = ESC//"[91m", &
			fg_bold_bright_red    = ESC//"[91;1m", &
			fg_bold_bright_yellow = ESC//"[93;1m", &
			fg_bright_green       = ESC//"[92m", &
			fg_bright_yellow      = ESC//"[93m", &
			fg_bright_blue        = ESC//"[94m", &
			fg_bright_magenta     = ESC//"[95m", &
			fg_bright_cyan        = ESC//"[96m", &
			fg_bright_white       = ESC//"[97m", &
			color_reset           = ESC//"[0m"

	character(len = *), parameter :: &
		ERROR_STR = fg_bold_bright_red   //"Error"  //fg_bold//": "//color_reset, &
		WARN_STR  = fg_bold_yellow//"Warning"//fg_bold//": "//color_reset

	!********

	interface to_str
		procedure :: to_str_i32
		procedure :: to_str_i64
	end interface to_str

	!********

	type str_t
		character(len = :), allocatable :: str
	end type str_t

	!********

	type str_builder_t
		! This is basically a dynamic char vector, but the type is a str and not
		! an actual array of single chars
		character(len = :), allocatable :: str
		integer(kind = 8) :: len, cap
		contains
			procedure :: &
				push => push_str_builder, &
				trim => trim_str_builder
	end type str_builder_t

	!********

	type str_vec_t
		type(str_t), allocatable :: vec(:)
		integer(kind = 8) :: len, cap
		contains
			procedure :: push => push_str
	end type str_vec_t

	!********

contains

!===============================================================================

function new_str_builder() result(sb)

	type(str_builder_t) :: sb

	sb%len = 0
	sb%cap = 16

	allocate(character(len = sb%cap) :: sb%str)

end function new_str_builder

!===============================================================================

function new_str_vec() result(vec)

	type(str_vec_t) :: vec

	vec%len = 0
	vec%cap = 2

	allocate(vec%vec( vec%cap ))

end function new_str_vec

!===============================================================================

subroutine print_str_vec(msg, sv)
	character(len = *), intent(in) :: msg
	type(str_vec_t) :: sv
	!********
	integer(kind = 8) :: i
	integer, parameter :: unit_ = output_unit

	write(unit_, "(a)") " "//msg
	write(unit_, *) "["
	do i = 1, sv%len
		write(unit_, "(a)") '     "'//sv%vec(i)%str//'",'
	end do
	write(unit_, *) "]"
end subroutine print_str_vec

!===============================================================================

function trim_str_builder(sb) result(str)

	class(str_builder_t), intent(in) :: sb
	character(len = :), allocatable :: str

	str = sb%str(1: sb%len)

end function trim_str_builder

!===============================================================================

subroutine push_str_builder(sb, val)

	! Push one char at a time onto a str builder.  Could generalize to push a
	! whole primitive str of len > 1

	class(str_builder_t), intent(inout) :: sb
	character, intent(in) :: val

	!********

	character(len = :), allocatable :: tmp

	integer(kind = 8) :: tmp_cap

	!print *, "pushing """//val//""""

	sb%len = sb%len + 1
	if (sb%len > sb%cap) then
		!print *, 'growing str'

		! Grow the buffer capacity.  What is the optimal growth factor?
		tmp_cap = 2 * sb%cap
		allocate(character(len = tmp_cap) :: tmp)
		tmp(1: sb%cap) = sb%str

		call move_alloc(tmp, sb%str)
		sb%cap = tmp_cap

	end if
	sb%str(sb%len: sb%len) = val

end subroutine push_str_builder

!===============================================================================

subroutine push_str(vec, val)

	class(str_vec_t), intent(inout) :: vec

	character(len = *), intent(in) :: val

	!********

	type(str_t) :: val_str
	type(str_t), allocatable :: tmp(:)

	integer(kind = 8) :: tmp_cap

	!print *, "pushing """//val//""""

	vec%len = vec%len + 1

	if (vec%len > vec%cap) then
		!print *, 'growing vec'

		tmp_cap = 2 * vec%len
		allocate(tmp( tmp_cap ))
		tmp(1: vec%cap) = vec%vec

		call move_alloc(tmp, vec%vec)
		vec%cap = tmp_cap

	end if

	val_str%str = val
	vec%vec( vec%len ) = val_str

end subroutine push_str

!===============================================================================

function count_lines(filename) result(nline)
	character(len=*), intent(in) :: filename
	integer :: nline
	!********
	character :: c
	integer :: iu, io

	nline = 0
	open(newunit = iu, file = filename, action = "read")
	do
		read(iu, "(a)", iostat = io) c
		! TODO: test on file with blank lines. May need to check io /= iostat_end
		if (io /= EXIT_SUCCESS) exit
		nline = nline + 1
	end do

end function count_lines

!===============================================================================

function read_mat_char(filename) result(mat)
	character(len=*), intent(in) :: filename
	character, allocatable :: mat(:,:)
	!********
	integer :: nx, ny, iu, x, y
	character(len=:), allocatable :: line

	ny = count_lines(filename)
	open(newunit = iu, file = filename, action = "read")
	line = read_line(iu)
	nx = len(line)
	!print *, "nx = ", nx

	allocate(mat(nx, ny))
	!mat = "."
	do y = 1, ny
		do x = 1, nx
			mat(x,y) = line(x:x)
		end do

		line = read_line(iu)
	end do
	!print *, "mat = ", mat

end function read_mat_char

!===============================================================================

function read_line(iu, iostat) result(str)

	! c.f. aoc-2022/utils.f90 and syntran/src/utils.f90
	!
	! This version reads WITHOUT backspacing, so it works on stdin too

	integer, intent(in) :: iu
	integer, optional, intent(out) :: iostat

	character(len = :), allocatable :: str

	!********

	character :: c

	integer :: io

	type(str_builder_t) :: sb

	!print *, 'starting read_line()'

	! Read 1 character at a time until end
	sb = new_str_builder()
	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		!print *, "io = ", io
		!print *, "c = """, c, """"

		if (io == iostat_end) exit
		if (io == iostat_eor) exit

		! In syntran, calling readln() one more time after the initial EOF
		! causes an infinite loop for some reason without this
		if (io /= 0) exit

		!if (c == carriage_return) exit
		!if (c == line_feed) exit

		call sb%push(c)

	end do
	str = sb%trim()

	!print *, "sb  = ", sb%str( 1: sb%len )
	!print *, "str = ", str

	!if (io == iostat_end .or. io == iostat_eor) io = 0
	if (io == iostat_eor) io = 0
	if (present(iostat)) iostat = io

end function read_line

!===============================================================================

function split(str, delims) result(strs)

	! This was translated from aoc-syntran and there are lots of off-by-one
	! differences going from syntran to fortran

	character(len = *), intent(in) :: str
	character(len = *), intent(in) :: delims
	type(str_vec_t) :: strs
	!********
	integer :: i, i0, n

	strs = new_str_vec()

	n = len(str)
	if (n == 0) return

	i = 1
	do while (i <= n)

		i0 = verify(str(i:n), delims) + i - 1
		if (i0 < i) i0 = n + 1

		i  = scan(str(i0:n), delims) + i0 - 1
		if (i < i0) i = n + 1

		if (i0 < i) call strs%push(str(i0: i - 1))
	end do

end function split

!===============================================================================
subroutine unit_test_split()
	character(len = :), allocatable :: str
	integer(kind = 8) :: i
	type(str_vec_t) :: strs
	str = "0,12,23,34,,7,,,,45,56,,1"
	strs = split(str, ",")
	!print *, "strs = ", strs%vec(:)%str
	!print *, "strs = ", [(strs%vec(i)%str, i = 1, strs%len)]
	print *, "strs = "//LINE_FEED//"["
	print "(a)", [(TAB//'"'//strs%vec(i)%str//'",', i = 1, strs%len)]
	print "(a)", "]"
	call exit(0)
end subroutine unit_test_split
!===============================================================================

function to_str_i32(int_) result(str)
	integer(kind = 4), intent(in) :: int_
	character(len = :), allocatable :: str
	character :: buffer*16
	write(buffer, "(i0)") int_
	str = trim(buffer)
end function to_str_i32

!===============================================================================

function to_str_i64(int_) result(str)
	integer(kind = 8), intent(in) :: int_
	character(len = :), allocatable :: str
	character :: buffer*16
	write(buffer, "(i0)") int_
	str = trim(buffer)
end function to_str_i64

!===============================================================================

integer function count_str_match(str_, char_) result(n)
	! Count the number `n` of characters `char_` in string `str_`
	character(len = *), intent(in) :: str_
	character, intent(in) :: char_
	!********
	integer :: i
	n = 0
	do i = 1, len(str_)
		if (str_(i:i) == char_) n = n + 1
	end do
end function count_str_match

!===============================================================================

subroutine print_mat_f32(msg, a)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	!
	! TODO: optional output file unit arg
	character(len = *), intent(in) :: msg
	real, intent(in) :: a(:,:)
	!********
	integer :: i, j, m, n
	integer, parameter :: unit_ = output_unit
	m = size(a,1)
	n = size(a,2)
	write(unit_, "(a)") " " // msg
	do i = 1, m
		do j = 1, n
			write(*, "(es11.3)", advance = "no") a(i,j)
		end do
		write(unit_, *)
	end do

end subroutine print_mat_f32

!===============================================================================

subroutine print_mat_i32(msg, a)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	!
	! TODO: optional output file unit arg
	character(len = *), intent(in) :: msg
	integer, intent(in) :: a(:,:)
	!********
	integer :: i, j, m, n
	integer, parameter :: unit_ = output_unit
	m = size(a,1)
	n = size(a,2)
	write(unit_, "(a)") " " // msg
	do i = 1, m
		do j = 1, n
			write(*, "(i6)", advance = "no") a(i,j)
		end do
		write(unit_, *)
	end do

end subroutine print_mat_i32

!===============================================================================

subroutine print_mat_i64(msg, a, width)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	!
	! TODO: optional int width arg, optional output file unit arg
	character(len = *), intent(in) :: msg
	integer(kind=8), intent(in) :: a(:,:)
	integer, optional, intent(in) :: width
	!********
	integer :: i, j, m, n, width_
	integer, parameter :: unit_ = output_unit

	! This could make two passes over the array to get the max width required,
	! or even max width per-column
	width_ = 6
	if (present(width)) width_ = width

	m = size(a,1)
	n = size(a,2)
	write(unit_, "(a)") " " // msg
	do i = 1, m
		do j = 1, n
			write(*, "(i"//to_str(width_)//")", advance = "no") a(i,j)
		end do
		write(unit_, *)
	end do

end subroutine print_mat_i64

!===============================================================================

subroutine print_mat_char(msg, a, transpose_)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	!
	! TODO: optional output file unit arg
	character(len = *), intent(in) :: msg
	character, intent(in) :: a(:,:)
	logical, optional, intent(in) :: transpose_
	!********
	integer :: i, j, m, n
	integer, parameter :: unit_ = output_unit
	logical :: transpose__
	transpose__ = .false.
	if (present(transpose_)) transpose__ = transpose_
	m = size(a,1)
	n = size(a,2)
	write(unit_, "(a)") " " // msg

	if (transpose__) then
		do i = 1, m
			! TODO: unroll inner loops for all print_mat*() fns
			do j = 1, n
				write(*, "(a)", advance = "no") a(i,j)
			end do
			write(unit_, *)
		end do
		return
	end if

	do j = 1, n
		do i = 1, m
			write(*, "(a)", advance = "no") a(i,j)
		end do
		write(unit_, *)
	end do

end subroutine print_mat_char

!===============================================================================

subroutine panic(msg)
	character(len = *), intent(in) :: msg
	if (msg /= "") write(*,*) ERROR_STR//msg
	call aoc_exit(EXIT_FAILURE)
end subroutine panic

!===============================================================================

subroutine aoc_exit(exit_code)
	integer, intent(in) :: exit_code
	if (exit_code == EXIT_SUCCESS) write(*,*) fg_bright_green//"Finished Fortran AOC"//color_reset
	call exit(exit_code)
end subroutine aoc_exit

!===============================================================================

logical function is_str_eq(a, b)
	! Fortran considers spaces as insignificant in str comparisons, but no sane
	! language would allow that
	!
	! I guess this is an artifact of fixed-length strings being common in older
	! fortran code
	character(len = *), intent(in) :: a, b
	!is_str_eq = a == b  ! not what you expect!

	is_str_eq = &
		len(a) == len(b) .and. &
		    a  ==     b
end function is_str_eq

!===============================================================================

function read_i32(str) result(a)
	character(len=*), intent(in) :: str
	integer(kind=4) :: a
	read(str, *) a
end function read_i32

!===============================================================================

function read_i32_delims(str, delims) result(v)
	character(len=*), intent(in) :: str, delims
	integer, allocatable :: v(:)
	!********
	integer(kind=8) :: i, n
	type(str_vec_t) :: strs

	strs = split(str, delims)
	n = strs%len
	allocate(v(n))
	do i = 1, n
		!read(strs%vec(i)%str, *) v(i)
		v(i) = read_i32(strs%vec(i)%str)
	end do

end function read_i32_delims

!===============================================================================

function i32(i)
	! Cast i64 down to i32
	integer(kind=8), intent(in) :: i
	integer(kind=4) :: i32
	i32 = int(i, 4)
end function i32

!===============================================================================

end module utils_m

!===============================================================================

