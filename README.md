
# aoc-fortran

Advent of code in Fortran (again)

## Plan

- try generic fns with unlimited polymorphic class(*) args
  * otherwise, overload things like print_mat_i32() and print_mat_f32() with a
    unified interface
- utils
  * Unit tests for utils
- Add README notes:
  * links to other aoc repos (syntran, older fortran)
  * how to run things in this repo
- Built-in timer instead of shell "time"

Soon:
- cmd arg parsing
  * --color never|auto|always (syntran has this)
- Prototypical advent of code problems:
  * Dijkstra problem (minheap)

## References and links

- Tsoding on generics:  https://www.youtube.com/watch?v=oEL9x6pP4FM&t=42s

