module map_m
	! This module has hash map classes with string keys and 32-bit integer
	! values
	!
	! TBD other value types, at least string for AOC

	implicit none

	type map_entry_i32_t
		character(len = :), allocatable :: key
		integer(kind = 4) :: val
	end type map_entry_i32_t

	type map_entry_str_t
		character(len = :), allocatable :: key
		character(len = :), allocatable :: val
	end type map_entry_str_t

	!********

	type map_i32_t
		type(map_entry_i32_t), allocatable :: entries(:)
		integer :: len_ = 0  ! TODO: choose consistent int type for len/cap
		contains
			procedure :: set => set_map_i32
			procedure :: get => get_map_i32
	end type map_i32_t

	type map_str_t
		type(map_entry_str_t), allocatable :: entries(:)
		integer :: len_ = 0
		contains
			procedure :: set => set_map_str
			procedure :: get => get_map_str
	end type map_str_t

contains

	function djb2_hash(str) result(hash)
		! DJB2 hash function implementation
		character(len=*), intent(in) :: str
		integer(8) :: hash
		integer :: i

		hash = 5381
		!do i = 1, len_trim(str)
		do i = 1, len(str)
			hash = ((hash * 33) + iachar(str(i:i)))  ! hash * 33 + c
		end do
	end function djb2_hash

	function new_map_i32(cap) result(map)
		! Create a new empty map
		type(map_i32_t) :: map
		integer(8), intent(in), optional :: cap
		integer(8) :: cap_
		map%len_ = 0
		cap_ = 256
		if (present(cap)) then
			cap_ = cap
		end if
		allocate(map%entries(cap_))
	end function new_map_i32

	recursive subroutine set_map_i32(this, key, val)
		! Set a key-value pair in the map
		class(map_i32_t), intent(inout) :: this
		character(len=*), intent(in) :: key
		integer(4), intent(in) :: val
		integer(8) :: i
		integer(8) :: hash, idx

		if (this%len_ * 2 >= size(this%entries)) then
		resize_block: block
			! Resize the entries array if load factor exceeds 0.5
			integer(8) :: new_size, old_size
			type(map_entry_i32_t), allocatable :: old_entries(:)
			old_size = size(this%entries)
			new_size = old_size * 2
			old_entries = this%entries
			deallocate(this%entries)
			allocate(this%entries(new_size))
			!this%entries = map_entry_i32_t()  ! Reset all entries
			!this%entries(:)%key = ""  ! reset all entries TODO?
			this%len_ = 0
			do i = 1, old_size
				if (allocated(old_entries(i)%key)) then
					! Why is this recursive?
					call this%set(old_entries(i)%key, old_entries(i)%val)
				end if
			end do
			deallocate(old_entries)
		end block resize_block
		end if

		hash = djb2_hash(key)
		idx = mod(hash, size(this%entries)) + 1
		do
			if (.not. allocated(this%entries(idx)%key)) then
				! Empty slot found, insert new entry
				!allocate(character(len=len_trim(key)) :: this%entries(idx)%key)
				this%entries(idx)%key = key
				this%entries(idx)%val = val
				this%len_ = this%len_ + 1
				exit
			else if (this%entries(idx)%key == key) then
				! Key already exists, update value
				this%entries(idx)%val = val
				exit
			else
				! Collision, try next index (linear probing)
				idx = mod(idx, size(this%entries)) + 1
			end if
		end do

	end subroutine set_map_i32

	function get_map_i32(this, key, found) result(val)
		! Get a value by key from the map
		class(map_i32_t), intent(in) :: this
		character(len=*), intent(in) :: key
		logical, intent(out), optional :: found
		integer(4) :: val
		integer(8) :: hash, idx
		logical :: found_
		found_ = .false.
		val = 0  ! Default value if not found
		hash = djb2_hash(key)
		idx = mod(hash, size(this%entries)) + 1
		do
			if (.not. allocated(this%entries(idx)%key)) then
				! Empty slot, key not found
				exit
			else if (this%entries(idx)%key == key) then
				! Key found
				val = this%entries(idx)%val
				found_ = .true.
				exit
			else
				! Collision, try next index (linear probing)
				idx = mod(idx, size(this%entries)) + 1
			end if
		end do
		if (present(found)) found = found_
	end function get_map_i32

	function new_map_str(cap) result(map)
		! Create a new empty map
		type(map_str_t) :: map
		integer(8), intent(in), optional :: cap
		integer(8) :: cap_
		map%len_ = 0
		cap_ = 256
		if (present(cap)) then
			cap_ = cap
		end if
		allocate(map%entries(cap_))
	end function new_map_str

	recursive subroutine set_map_str(this, key, val)
		! Set a key-value pair in the map
		class(map_str_t), intent(inout) :: this
		character(len=*), intent(in) :: key
		character(len=*), intent(in) :: val
		integer(8) :: i
		integer(8) :: hash, idx

		if (this%len_ * 2 >= size(this%entries)) then
		resize_block: block
			! Resize the entries array if load factor exceeds 0.5
			integer(8) :: new_size, old_size
			type(map_entry_str_t), allocatable :: old_entries(:)
			old_size = size(this%entries)
			new_size = old_size * 2
			old_entries = this%entries
			deallocate(this%entries)
			allocate(this%entries(new_size))
			!this%entries = map_entry_str_t()  ! Reset all entries
			!this%entries(:)%key = ""  ! reset all entries TODO?
			this%len_ = 0
			do i = 1, old_size
				if (allocated(old_entries(i)%key)) then
					! Why is this recursive?
					call this%set(old_entries(i)%key, old_entries(i)%val)
				end if
			end do
			deallocate(old_entries)
		end block resize_block
		end if

		hash = djb2_hash(key)
		idx = mod(hash, size(this%entries)) + 1
		do
			if (.not. allocated(this%entries(idx)%key)) then
				! Empty slot found, insert new entry
				!allocate(character(len=len_trim(key)) :: this%entries(idx)%key)
				this%entries(idx)%key = key
				this%entries(idx)%val = val
				this%len_ = this%len_ + 1
				exit
			else if (this%entries(idx)%key == key) then
				! Key already exists, update value
				this%entries(idx)%val = val
				exit
			else
				! Collision, try next index (linear probing)
				idx = mod(idx, size(this%entries)) + 1
			end if
		end do

	end subroutine set_map_str

	function get_map_str(this, key, found) result(val)
		! Get a value by key from the map
		class(map_str_t), intent(in) :: this
		character(len=*), intent(in) :: key
		logical, intent(out), optional :: found
		character(len = :), allocatable :: val
		integer(8) :: hash, idx
		logical :: found_
		found_ = .false.
		val = ""  ! Default value if not found
		hash = djb2_hash(key)
		idx = mod(hash, size(this%entries)) + 1
		do
			if (.not. allocated(this%entries(idx)%key)) then
				! Empty slot, key not found
				exit
			else if (this%entries(idx)%key == key) then
				! Key found
				val = this%entries(idx)%val
				found_ = .true.
				exit
			else
				! Collision, try next index (linear probing)
				idx = mod(idx, size(this%entries)) + 1
			end if
		end do
		if (present(found)) found = found_
	end function get_map_str

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
!   ! TODO: i think this only works by coincidence. From experience with a
!   ! syntran bug, i know fortran thinks ' ' == '  ' is true, hence syntran has
!   ! a `is_str_eq()` fn in its source
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

