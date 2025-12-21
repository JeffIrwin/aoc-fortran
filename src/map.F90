module map_m
	! This module has hash map classes with string keys and various value types

	use map_i32_m
	use map_i64_m
	use map_str_m
	implicit none

!	!********
!	! Testing routines, not critical for core map functionality
!
!#define TEST(val, msg, nfail, ntot) call test_(val, msg, nfail, ntot, __FILE__, __LINE__)
!	subroutine test_(val, msg, nfail, ntot, file, line)
!		logical, intent(in) :: val
!		character(len=*), intent(in) :: msg
!		integer, intent(inout) :: nfail, ntot
!		character(len=*), intent(in) :: file
!		integer, intent(in) :: line
!
!		ntot = ntot + 1
!		if (.not. val) then
!			print *, RED // "Test failed: " // RESET
!			print *, "        ", trim(msg)
!			print *, "        at ", trim(file), ":", line
!			nfail = nfail + 1
!		end if
!	end subroutine test_
!
!	integer function rand_int() result(r)
!		! Simple linear congruential generator for random numbers
!		!
!		! TODO: use built-in rng, not this vibe-coded bs
!		integer, save :: seed = 123456789
!		seed = mod(1103515245 * seed + 12345, 2_8**31)
!		r = seed
!	end function rand_int
!
!	function rand_str(len_) result(str)
!		! Generate a random string of given length
!		integer, intent(in) :: len_
!		character(len=len_) :: str
!		integer :: i, j
!		character(len=62), parameter :: chars = &
!			"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
!		do i = 1, len_
!			j = modulo(rand_int(), 62) + 1
!			!print *, "j = ", j
!			str(i:i) = chars(j:j)
!		end do
!	end function rand_str

end module map_m

!program main
!	use map_m
!	implicit none
!
!	character(len = :), allocatable :: str
!	integer :: nfail, ntot, i
!	integer(4) :: val
!	logical :: found
!	type(map_i32_t) :: map
!
!	print *, MAGENTA // "starting map.f90 main" // RESET
!	nfail = 0
!	ntot = 0
!
!	str = "world"
!	print *, "Hash of '" // str // "' is: ", djb2_hash(str)
!
!	map = new_map_i32()
!	call map%set("foo", 1)
!	call map%set("bar", 2)
!	call map%set("baz", 3)
!	call map%set(""  , 10)
!	call map%set(" " , 11)
!	call map%set("  ", 12)
!
!	print *, "map['foo'] = ", map%get("foo")
!	print *, "map['bar'] = ", map%get("bar")
!	print *, "map['baz'] = ", map%get("baz", found)
!	print *, "    (found = ", found, ")"
!	print *, "map['barf'] = ", map%get("barf", found)
!	print *, "    (found = ", found, ")"
!
!	print *, "map[''] = ", map%get("")
!	print *, "map[' '] = ", map%get(" ")
!	print *, "map['  '] = ", map%get("  ")
!
!	TEST(map%get("foo") == 1, "map foo", nfail, ntot)
!	TEST(map%get("bar") == 2, "map bar", nfail, ntot)
!	TEST(map%get("baz") == 3, "map baz", nfail, ntot)
!	TEST(map%get("") == 10, "map ''", nfail, ntot)
!	TEST(map%get(" ") == 11, "map ' '", nfail, ntot)
!	TEST(map%get("  ") == 12, "map '  '", nfail, ntot)
!
!	val = map%get("bar", found)
!	TEST(found, "map bar found", nfail, ntot)
!	val = map%get("barf", found)
!	TEST(.not. found, "map barf not found", nfail, ntot)
!
!	!!str = rand_str(10)
!	!print *, "rand str = ", rand_str(10)
!	!print *, "rand str = ", rand_str(10)
!	!print *, "rand str = ", rand_str(10)
!
!	do i = 1, 256*256
!		str = rand_str(10)
!		call map%set(str, i)
!		val = map%get(str, found)
!		TEST(found, "random str found", nfail, ntot)
!		TEST(val == i, "random str value correct", nfail, ntot)
!	end do
!
!	if (nfail == 0) then
!		print "(a,i0,a)", BOLD // GREEN // " All ", ntot, " tests passed " // RESET
!	else
!		print "(a,i0,a,i0,a)", BOLD // RED // " Error: ", nfail, "/", ntot, " tests failed " // RESET
!	end if
!
!	print *, MAGENTA // "ending map.f90 main" // RESET
!
!end program main

