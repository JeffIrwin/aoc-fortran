
module sort_m

	use blarg_m
	implicit none

contains

!===============================================================================

function sortidx_i32(a) result(idx)
	integer(kind=4), intent(in) :: a(:)
	integer(kind=4), allocatable :: idx(:)
	!********
	integer(kind=4) :: n

	n = size(a)
	idx = range_i32(n)
	call sortidx_i32_sub(a, idx, 1, n)

end function sortidx_i32

!===============================================================================

recursive subroutine sortidx_i32_sub(a, idx, lo, hi)
	! Quicksort a rank-2 array a along its 2nd dimension and return the
	! sort permutation idx
	!
	! See also:  https://github.com/JeffIrwin/aoc-2022/blob/381da3c2d468b4e2a9d4bb1068d84a9e8ae6bec6/2022/23/main.f90#L114

	integer(kind=4), intent(in) :: a(:)
	integer(kind=4) :: idx(:)
	integer(kind=4), intent(in) :: lo, hi
	!********
	integer(kind=4) :: p

	if (lo >= hi .or. lo < 1) return
	p = partition_i32(a, idx, lo, hi)

	call sortidx_i32_sub(a, idx, lo, p - 1)
	call sortidx_i32_sub(a, idx, p + 1, hi)

end subroutine sortidx_i32_sub

!===============================================================================

function partition_i32(a, idx, lo, hi) result(ans)

	integer(kind=4), intent(in) :: a(:)
	integer(kind=4), intent(inout) :: idx(:)
	integer(kind=4) :: ans
	!********
	integer(kind=4) :: pivot
	integer(kind=4) :: lo, hi, i, j, mid

	! Median of three pivot
	mid = (lo + hi) / 2
	if (a(idx(mid)) < a(idx(lo))) then
		idx([lo, mid]) = idx([mid, lo])
	else if (a(idx(hi)) < a(idx(lo))) then
		idx([lo, hi]) = idx([hi, lo])
	else if (a(idx(mid)) < a(idx(hi))) then
		idx([mid, hi]) = idx([hi, mid])
	end if
	pivot = a(idx(hi))

	i = lo - 1
	do j = lo, hi - 1
		if (a(idx(j)) <= pivot) then
			i = i + 1
			idx([i, j]) = idx([j, i])
		end if
	end do

	i = i + 1
	idx([i, hi]) = idx([hi, i])
	ans = i

end function partition_i32

!===============================================================================

function sortidx_i64(a) result(idx)
	integer(kind=8), intent(in) :: a(:)
	integer(kind=8), allocatable :: idx(:)
	!********
	integer(kind=8) :: n

	n = size(a)
	idx = range_i64(n)
	call sortidx_i64_sub(a, idx, 1_8, n)

end function sortidx_i64

!===============================================================================

recursive subroutine sortidx_i64_sub(a, idx, lo, hi)
	! Quicksort a rank-2 array a along its 2nd dimension and return the
	! sort permutation idx
	!
	! See also:  https://github.com/JeffIrwin/aoc-2022/blob/381da3c2d468b4e2a9d4bb1068d84a9e8ae6bec6/2022/23/main.f90#L114

	integer(kind=8), intent(in) :: a(:)
	integer(kind=8) :: idx(:)
	integer(kind=8), intent(in) :: lo, hi
	!********
	integer(kind=8) :: p

	if (lo >= hi .or. lo < 1) return
	p = partition_i64(a, idx, lo, hi)

	call sortidx_i64_sub(a, idx, lo, p - 1)
	call sortidx_i64_sub(a, idx, p + 1, hi)

end subroutine sortidx_i64_sub

!===============================================================================

function partition_i64(a, idx, lo, hi) result(ans)

	integer(kind=8), intent(in) :: a(:)
	integer(kind=8), intent(inout) :: idx(:)
	integer(kind=8) :: ans
	!********
	integer(kind=8) :: pivot
	integer(kind=8) :: lo, hi, i, j, mid

	! Median of three pivot
	mid = (lo + hi) / 2
	if (a(idx(mid)) < a(idx(lo))) then
		idx([lo, mid]) = idx([mid, lo])
	else if (a(idx(hi)) < a(idx(lo))) then
		idx([lo, hi]) = idx([hi, lo])
	else if (a(idx(mid)) < a(idx(hi))) then
		idx([mid, hi]) = idx([hi, mid])
	end if
	pivot = a(idx(hi))

	i = lo - 1
	do j = lo, hi - 1
		if (a(idx(j)) <= pivot) then
			i = i + 1
			idx([i, j]) = idx([j, i])
		end if
	end do

	i = i + 1
	idx([i, hi]) = idx([hi, i])
	ans = i

end function partition_i64

!===============================================================================

end module sort_m

