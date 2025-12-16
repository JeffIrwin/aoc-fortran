
# aoc-fortran

Advent of code in Fortran (again)

## Plan

- Build system.  GNU make?
  * add `make debug` vs `make release` options
- AOC 2025 day 10, part 2, then part 1
- utils
  * Reading files, splitting strings, etc.
  * Copy from syntran, ribbit, numa (blarg.f90), (fynth?). Consolidate forks
  * Unit tests for utils
- Scripts from aoc-syntran: encryption, decryption, newday, run
- Add README note about other aoc repos

Later:
- cmd arg parsing
  * -t --test (input), full "real" input by default
  * -1, -2, or default both parts of AOC problem
  * --color never|auto|always (syntran has this)
- Prototypical advent of code problems:
  * Hash map problem
  * Dijkstra problem (minheap)
  * Recursion problem
- CI/CD with GitHub actions and/or Docker

