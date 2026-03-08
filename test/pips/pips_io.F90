
!> Module for reading and writing pips input data in JSON and text formats
module pips_io_m

	use aoc_all_m
	implicit none

	type pips_t
		! Pips game input data
		!
		! TODO: rename members
		character, allocatable :: char_grid(:,:), region_name(:), type_(:)
		integer :: nx, ny, num_regions, num_dominoes
		integer, allocatable :: target_(:)
		integer, allocatable :: domino(:,:)
	end type pips_t

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
	p%num_regions = 0
	p%num_dominoes = 0
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
			p%num_regions = p%num_regions + 1
		else if (i == 3) then
			p%num_dominoes = p%num_dominoes + 1
		else
			call panic("bad input -- too many blank lines")
		end if

	end do
	print *, "ny, nr, nd = ", p%ny, p%num_regions, p%num_dominoes
	rewind(iu)

	! Read the board
	str_ = read_line(iu, io)
	p%nx = len(str_)
	allocate(p%char_grid(p%nx, p%ny))
	p%char_grid = "."
	do y = 1, p%ny
		do x = 1, p%nx
			p%char_grid(x,y) = str_(x:x)
		end do

		str_ = read_line(iu, io)
	end do
	print *, "nx = ", p%nx
	call print_mat_char("cg = ", p%char_grid)

	! Read the region constraints, including names, types, and target values
	allocate(p%region_name(p%num_regions), p%type_(p%num_regions), p%target_(p%num_regions))
	p%type_ = "."  ! default type: sum equal to given digit
	p%target_ = -1
	do i = 1, p%num_regions
		str_ = read_line(iu, io)
		!print *, "str_ = ", str_

		p%region_name(i) = str_(1:1)  ! name: a-z
		if (.not. any(p%char_grid == p%region_name(i))) then
			call panic('constraint region "'//p%region_name(i)//'" does not exist on input board')
		end if

		c = str_(4:4)
		if (is_digit(c)) then
			! Default type
			p%target_(i) = read_i32(str_(4:))

		else if (c == ">") then
			p%type_(i) = c
			p%target_(i) = read_i32(str_(5:))

		else if (c == "<") then
			p%type_(i) = c
			p%target_(i) = read_i32(str_(5:))

		else if (c == "=") then
			p%type_(i) = c  ! no value for this type
			if (str_(4:) /= "=") then
				call panic('end-of-line junk found: "'//str_(4:)//'"')
			end if

		else if (c == "!") then
			p%type_(i) = c
			if (str_(4:) /= "!=") then
				call panic('end-of-line junk found: "'//str_(4:)//'"')
			end if

		else
			call panic("bad input -- unexpected region constraint type")
		end if

	end do
	str_ = read_line(iu, io)  ! skip blank line

	! TODO: move this to print_pips()
	print "(a,"//to_str(p%num_regions)//"a3)", " names   = ", p%region_name
	print "(a,"//to_str(p%num_regions)//"a3)", " types   = ", p%type_
	print "(a,"//to_str(p%num_regions)//"i3)", " targets = ", p%target_

	! TODO: more input sanity checks:
	! - no region rules for "*" or ".", maybe even alphabetic only?
	! - no pip counts <0 or >6
	! - square input board, padding required
	! - split on delims to allow extra whitespace
	! - allow comments?

	! Sanity check on total area
	if (2 * p%num_dominoes < count(p%char_grid /= ".")) then
		call panic("too few dominoes to cover board")
	else if (2 * p%num_dominoes > count(p%char_grid /= ".")) then
		call panic("too many dominoes to fit in board")
	end if

	! Read the dominoes
	p%domino = zeros_i32(2, p%num_dominoes)
	do i = 1, p%num_dominoes
		str_ = read_line(iu, io)
		!print *, "str_ = ", str_
		p%domino(:,i) = read_i32_delims(str_, ", ")
	end do
	close(iu)
	call print_mat_i32("ds (transpose) = ", p%domino)

end function read_pips_text

!===============================================================================

function read_pips_json(filename, difficulty) result(p)
	use jsonf
	character(len=*), intent(in) :: filename, difficulty
	type(pips_t) :: p
	!********
	integer :: nr, ir, jr, nx, ny, ii
	integer, allocatable :: indices(:,:)
	type(json_t) :: json
	character :: c
	character(len=:), allocatable :: rkey, type_

	write(*,*) 'Reading pips JSON file "'//filename//'" '//difficulty//' difficulty ...'
	select case (difficulty)
	case ("easy", "medium", "hard")
	case default
		call panic('bad difficulty "'//difficulty//'"')
	end select

	call json%read_file(filename)

	nx = -huge(nx)
	ny = -huge(ny)

	nr = json%len("/"//difficulty//"/regions")
	!print *, "nr = ", nr

	! Read the region constraints. JSON does not provide names, so make them
	! here named "A", "B", "C", ...
	allocate(p%region_name(nr), p%type_(nr), p%target_(nr))
	p%type_ = "."  ! default type: sum equal to given digit
	p%target_ = -1

	! First pass: save regions and get the size [nx, ny] of the game board grid
	jr = 0  ! non-empty region counter
	do ir = 1, nr
		! Iterate over all regions, including empty ones

		rkey = "/"//difficulty//"/regions/"//to_str(ir-1)
		!print *, "rkey = ", rkey

		type_ = json%get_str(rkey//"/type")
		!print *, "type_ = ", type_
		if (type_ /= "empty") then
			jr = jr + 1
			p%region_name(jr) = char(ichar("A") + jr - 1)
		end if
		select case (type_)
		case ("empty")
			! Do nothing. I don't explicitly save empty regions like others
		case ("sum")
			!p%type_(jr) = "."  ! default already initialized
			p%target_(jr) = int(json%get_i64(rkey//"/target"))
		case ("greater")
			p%type_(jr) = ">"
			p%target_(jr) = int(json%get_i64(rkey//"/target"))
		case ("less")
			p%type_(jr) = "<"
			p%target_(jr) = int(json%get_i64(rkey//"/target"))
		case ("equals")
			p%type_(jr) = "="
		case ("unequal")
			p%type_(jr) = "!"
		case default
			call panic('bad region type "'//type_//'"')
		end select

		indices = int(json%get_mat_i64(rkey//"/indices"))
		nx = max(nx, maxval(indices(:,1)))
		ny = max(ny, maxval(indices(:,2)))
	end do
	nx = nx + 1  ! convert 0-index to 1-index
	ny = ny + 1
	!print *, "nx, ny = ", nx, ny
	!print *, "rt = ", p%type_
	allocate(p%char_grid(nx, ny))
	p%char_grid = "."

	p%num_regions = jr
	p%region_name = p%region_name(1: p%num_regions)  ! trim empties
	p%type_ = p%type_(1: p%num_regions)
	p%target_ = p%target_(1: p%num_regions)

	! Second pass: save the grid
	jr = 0  ! non-empty region counter
	do ir = 1, nr
		rkey = "/"//difficulty//"/regions/"//to_str(ir-1)
		type_ = json%get_str(rkey//"/type")
		if (type_ == "empty") then
			c = "*"
		else
			jr = jr + 1
			c = p%region_name(jr)
		end if
		indices = int(json%get_mat_i64(rkey//"/indices"))
		do ii = 1, size(indices, 1)
			p%char_grid(indices(ii,1)+1, indices(ii,2)+1) = c
		end do
	end do

	! JSON input uses [row, col] convention, but I prefer [x, y]
	p%char_grid = transpose(p%char_grid)
	p%nx = ny
	p%ny = nx

	!call print_mat_char("cg = ", p%char_grid)

	! Parse the dominoes
	p%domino = transpose(int(json%get_mat_i64("/"//difficulty//"/dominoes")))
	p%num_dominoes = size(p%domino, 2)
	!call print_mat_i32("ds (transpose) = ", p%domino)

end function read_pips_json

!===============================================================================

subroutine write_pips_text(filename, pips)
	! Write a `pips` game struct to a text file
	character(len=*), intent(in) :: filename
	type(pips_t), intent(in) :: pips
	!********
	integer :: unit_, x, y, i

	open(newunit = unit_, file = filename, action = "write")
	! TODO: write a comment in the header after I can support that in the reader

	! Write the game board grid
	do y = 1, pips%ny
		do x = 1, pips%nx
			write(unit_, "(a)", advance = "no") pips%char_grid(x,y)
		end do
		write(unit_, *)
	end do
	write(unit_, *)

	! Write the region constraints
	do i = 1, pips%num_regions
		write(unit_, "(a)", advance = "no") pips%region_name(i)//": "
		if (pips%type_(i) /= ".") then
			write(unit_, "(a)", advance = "no") pips%type_(i)
		end if
		if (.not. any(pips%type_(i) == ["=", "!"])) then
			write(unit_, "(i0)", advance = "no") pips%target_(i)
		end if
		write(unit_, *)
	end do
	write(unit_, *)

	! Write the dominoes
	do i = 1, pips%num_dominoes
		write(unit_, "(i0, ',', i0)") pips%domino(:,i)
	end do

	close(unit_)
	write(*,*) 'Finished writing pips text file "'//filename//'"'
	write(*,*)

end subroutine write_pips_text

!===============================================================================

end module pips_io_m

