
# aoc-fortran

Advent of code in Fortran (again)

## Plan

- Build system
  * add `make debug` vs `make release` options
- utils
  * Copy from syntran, ribbit, numa (blarg.f90), (fynth?). Consolidate forks
  * Unit tests for utils
- Add README note about other aoc repos
- Update newday and run scripts for Fortran

Soon:
- cmd arg parsing
  * -t --test (input), full "real" input by default
  * -1, -2, or default both parts of AOC problem
  * --color never|auto|always (syntran has this)
- Prototypical advent of code problems:
  * Sorting routines -- index array, in-place, type overloads
  * Grid problem and reading character matrices
  * Hash map problem
  * Dijkstra problem (minheap)
  * Recursion problem

Much later:
- Docker
- CI/CD with GitHub actions

## References and links

- Tsoding on generics:  https://www.youtube.com/watch?v=oEL9x6pP4FM&t=42s

