
# NYT pips solver

This directory contains a solver program for the New York Times *pips* puzzle.

## Input

The program takes a bespoke text file format as its input.  For example, see
[easy-2025-12-28.txt](easy-2025-12-28.txt):

```text
.ab
..b
..c
dcc
eef

a: 5
b: =
c: =
d: 5
e: =
f: 4

5,6
5,3
2,2
3,4
6,3
```

## Run

Run the program by providing the `--input` argument:
```shell
./run.sh release --input easy-2025-12-28.txt
```

## Results

The answer is printed to the console:
```text
 answer =
   5-6

     6
     |
     3

 5-3 3
     |
 2-2 4
```

A `-` character represents a horizontal domino with the pip counts shown at
either end, e.g. `5-6`, while a `|` character represents a vertical domino.

