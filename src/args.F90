module args_m

	use utils
	implicit none

	type args_t

		logical :: &
			test               = .false., &  ! use test input file? otherwise full input
			assert             = .false., &  ! assert correctness of results?
			part1              = .false., &  ! run part 1 only? default both parts
			part2              = .false., &
			has_input_filename = .false.

		character(len = :), allocatable :: input_filename

	end type args_t

contains

function parse_args() result(args)
	type(args_t) :: args
	!********
	character(len = :), allocatable :: arg!, arg0
	integer :: i, nargs, len_, io, ipos
	logical :: error = .false.
	type(str_vec_t) :: argv

	! Set defaults
	args%input_filename = "input.txt"

	! Get the cmd args as a vector of strings
	nargs = command_argument_count()
	argv = new_str_vec()
	do i = 1, nargs

		call get_command_argument(i, length = len_, status = io)
		! TODO: handle io

		!print *, "arg "//to_str(i)//" len_ = ", len_

		allocate(character(len = len_) :: arg)

		call get_command_argument(i, value = arg, status = io)
		! TODO: handle io

		!print *, "arg "//to_str(i)//" = ", arg

		call argv%push(arg)
		deallocate(arg)
	end do
	!call print_str_vec("argv = ", argv)

	! Parse the args
	i = 0
	ipos = 0
	!arg0 = ""
	do while (i < nargs)
		i = i + 1
		arg = argv%vec(i)%str
		print *, "arg = ", arg

		select case (arg)
		!case ("-h", "-help", "--help")  ! TODO
		case ("-t", "--test")
			args%test = .true.
			args%input_filename = "test-input.txt"

		case ("-a", "--assert")
			args%assert = .true.

		case ("-1", "--part1")
			args%part1 = .true.

		case ("-2", "--part2")
			args%part2 = .true.

		case ("-i", "--input")
			i = i + 1
			args%has_input_filename = .true.
			args%input_filename = argv%vec(i)%str
			print *, "input filename = ", args%input_filename

		case default
			! run.sh passes args to make as well as this program. don't
			! error-out because we can't understand make's args
			write(*,*) WARN_STR//'bad command argument "'//arg//'"'

		end select

		!arg0 = arg
	end do

	if (args%test .and. args%has_input_filename) then
		write(*,*) ERROR_STR//'command arguments "--test" and "--input" cannot both be used'
		error = .true.
	end if

	!********

	if (error) then
		! TODO: log help/usage message

		!call panic("bad command syntax")
		call panic("")
	end if

end function parse_args

end module args_m

