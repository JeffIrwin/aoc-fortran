
# aoc-fortran

Advent of code in Fortran (again)

## Plan

- try generic fns with unlimited polymorphic class(*) args
  * otherwise, overload things like print_mat_i32() and print_mat_f32() with a
    unified interface
- utils
  * Copy from syntran, ribbit, numa (blarg.f90), (fynth?). Consolidate forks
  * Unit tests for utils
- Add README notes:
  * links to other aoc repos (syntran, older fortran)
  * how to run things in this repo
- Update newday and run scripts for Fortran
- Built-in timer instead of shell "time"

Soon:
- cmd arg parsing
  * --color never|auto|always (syntran has this)
- Prototypical advent of code problems:
  * Sorting routines -- index array, in-place, type overloads
  * Grid problem and reading character matrices
  * Dijkstra problem (minheap)
  * Recursion problem

Much later:
- Docker
- CI/CD with GitHub actions
- Experiment with fpm (or cmake) as a build system
  * FPM has a "local dependency" option that I should be able to use to build my
    lib from a separate fpm.toml in its own dir.
  * Make has been underwhelming. I have to clean in between switching
    debug/release or else have a very long, verbose, error-prone makefile

## References and links

- Tsoding on generics:  https://www.youtube.com/watch?v=oEL9x6pP4FM&t=42s

