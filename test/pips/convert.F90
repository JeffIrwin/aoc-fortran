
module convert_m

	use json_module
	implicit none

	type pips_t
		! Pips game input data
		character, allocatable :: cg(:,:), rl(:), rt(:)
		integer :: nx, ny, nr, nd
		integer, allocatable :: rv(:)
		integer, allocatable :: ds(:,:)
	end type pips_t

contains

subroutine json_hello_world()
	type(json_file) :: json

	call json%deserialize('{"name": "Leonidas"}')

	!print *, "json.name = ", json%get("name")
	call json%print()

end subroutine json_hello_world

subroutine convert(input_filename, output_filename)
	use utils_m
	character(len=*), intent(in) :: input_filename, output_filename
	!********
	integer :: nr, nd, ni, ir, jr, nx, ny, x, y, ii, id
	type(json_file) :: json
	character :: c
	character(len=:), allocatable :: rkey, ikey, dkey, type_
	type(pips_t) :: p

	call json%load(input_filename)

	!call json%print()
	!call json%

	! TODO: refactor as a subroutine to share with easy, medium, and hard
	! puzzles all in one input json

	nx = -huge(nx)
	ny = -huge(ny)

	call json%info("easy.regions", n_children = nr)
	print *, "nr = ", nr

	! Read the region constraints, including labels `rl`, types `rt`, and values
	! `rv`. JSON does not provide labels, so make them here named "A", "B", "C",
	! ...
	allocate(p%rl(nr), p%rt(nr), p%rv(nr))
	p%rt = "."  ! default type: sum equal to given digit
	p%rv = -1

	! First pass: save regions and get the size [nx, ny] of the game board grid
	jr = 0  ! non-empty region counter
	do ir = 1, nr
		! Iterate over all regions, including empty ones

		rkey = "easy.regions["//to_str(ir)//"]"
		print *, "rkey = ", rkey

		call json%get(rkey//".type", type_)
		print *, "type_ = ", type_
		if (type_ /= "empty") then
			jr = jr + 1
			p%rl(jr) = char(ichar("A") + jr - 1)
		end if
		select case (type_)
		case ("empty")
			! Do nothing. I don't explicitly save empty regions like others
		case ("sum")
			!p%rt(jr) = "."  ! default already initialized
			call json%get(rkey//".target", p%rv(jr))
		case ("greater")
			p%rt(jr) = ">"
			call json%get(rkey//".target", p%rv(jr))
		case ("less")
			p%rt(jr) = "<"
			call json%get(rkey//".target", p%rv(jr))
		case ("equals")
			p%rt(jr) = "="
		case ("unequal")
			p%rt(jr) = "!"
		case default
			call panic('bad region type "'//type_//'"')
		end select

		call json%info(rkey//".indices", n_children = ni)
		print *, "ni = ", ni

		do ii = 1, ni
			! Iterate over indices
			ikey = rkey//".indices["//to_str(ii)//"]"
			call json%get(ikey//"[1]", x)
			call json%get(ikey//"[2]", y)
			print *, "x, y = ", x, y

			nx = max(nx, x)
			ny = max(ny, y)
		end do
	end do
	nx = nx + 1  ! convert 0-index to 1-index
	ny = ny + 1
	print *, "nx, ny = ", nx, ny
	!print *, "rt = ", p%rt
	allocate(p%cg(nx, ny))
	p%cg = "."

	p%nr = jr
	p%rl = p%rl(1: p%nr)  ! trim empties
	p%rt = p%rt(1: p%nr)
	p%rv = p%rv(1: p%nr)

	print "(a,"//to_str(nr)//"a3)", " rl = ", p%rl
	print "(a,"//to_str(nr)//"a3)", " rt = ", p%rt
	print "(a,"//to_str(nr)//"i3)", " rv = ", p%rv

	! Second pass: save the grid
	jr = 0  ! non-empty region counter
	do ir = 1, nr
		rkey = "easy.regions["//to_str(ir)//"]"
		call json%get(rkey//".type", type_)
		if (type_ == "empty") then
			c = "*"
		else
			jr = jr + 1
			c = p%rl(jr)
		end if
		call json%info(rkey//".indices", n_children = ni)
		do ii = 1, ni
			! Iterate over indices
			ikey = rkey//".indices["//to_str(ii)//"]"
			call json%get(ikey//"[1]", x)
			call json%get(ikey//"[2]", y)
			!print *, "x, y = ", x, y
			p%cg(x+1, y+1) = c
		end do
	end do

	! JSON input uses [row, col] convention, but I prefer [x, y]
	p%cg = transpose(p%cg)
	p%nx = ny
	p%ny = nx

	call print_mat_char("cg = ", p%cg)

	! Parse the dominoes
	call json%info("easy.dominoes", n_children = nd)
	print *, "nd = ", nd
	allocate(p%ds(2, nd))
	do id = 1, nd
		dkey = "easy.dominoes["//to_str(id)//"]"
		call json%get(dkey//"[1]", p%ds(1, id))
		call json%get(dkey//"[2]", p%ds(2, id))
	end do
	p%nd = nd
	call print_mat_i32("ds (transpose) = ", p%ds)

	! TODO: check json errors and print message, or set it to stop on error (if
	! that also makes it print by default)

	call write_pips_text(output_filename, p)

end subroutine convert

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
			write(unit_, "(a)", advance = "no") pips%cg(x,y)
		end do
		write(unit_, *)
	end do
	write(unit_, *)

	! Write the region constraints
	do i = 1, pips%nr
		write(unit_, "(a)", advance = "no") pips%rl(i)//": "
		if (pips%rt(i) /= ".") then
			write(unit_, "(a)", advance = "no") pips%rt(i)
		end if
		if (.not. any(pips%rt(i) == ["=", "!"])) then
			write(unit_, "(i0)", advance = "no") pips%rv(i)
		end if
		write(unit_, *)
	end do
	write(unit_, *)

	! Write the dominoes
	do i = 1, pips%nd
		write(unit_, "(i0, ',', i0)") pips%ds(:,i)
	end do

	close(unit_)
	write(*,*) 'Finished writing pips text file "'//filename//'"'

end subroutine write_pips_text

end module convert_m

program main
	use convert_m
	implicit none
	character(len=:), allocatable :: input_filename, output_filename
	character :: buffer*1024

	print *, "Starting convert main"

	call json_hello_world()

	call get_command_argument(1, value = buffer)!, status = io) ! TODO: just use args.F90
	input_filename = trim(buffer)
	print *, 'input_filename = "' // input_filename // '"'

	call get_command_argument(2, value = buffer)!, status = io) ! TODO: just use args.F90
	output_filename = trim(buffer)
	print *, 'output_filename = "' // output_filename // '"'

	call convert(input_filename, output_filename)

	print *, "Ending convert main"
end program main

