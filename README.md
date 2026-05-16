# Classic Computer Science Problems in Haskell

This repository contains my attempt at porting the source code of the books *Classic Computer Science Problems in Python* and *Classic Computer Science Problems in Java* by David Kopec to Haskell.

The source code is organized by chapter, treating each chapter as a package and code meant to be 'run' as an executable within that package. The executable name matches the name of the example in the book, e.g. `fib1`, `fib2` etc. as used in chapters 1 of the books.

If you installed Haskell using [GHCup](https://www.haskell.org/ghcup/), thus have cabal installed, you can compile and run the examples in a terminal by cd-ing into the corresponding chapter folder and executing `cabal run <name of the example>`, for example `cd chapter1` `cabal run fib2`.  
You can also run the examples using the interactive Haskell interpreter. To do so, cd into the corresponding chapter folder and
start `cabal repl <example>`, e.g. `cabal repl Fib2`.
Execute the example by calling the `main` function after the example is loaded into the interactive interpreter. You can also try the other functions defined in the example, of course.

If the example has no build dependency other than `base` (see line `build-depends:` of the corresponding example in the chapter\<n>.cabal file), you can also simply start `ghci` and load the example using `:load <Example>`, e.g. `:load Fib2` (note that the file names start with an uppercase character), or start `ghci` in the root folder and load the example using `:load chapter<n>/<Example>`.
In any case, you can execute the example by calling the `main` function after the example is loaded into the interactive interpreter.  

Code meant for reuse such as `GenericSearch` of chapter 2 and `csp` of chapter 3 is treated as a library.

Done:

- Chapter 1:
  - Fibonacci series, examples fib1, fib2, fib3, fib4, fib5, fib6
  - Trivial (gene) compression - trivialCompression
  - unbreakableEncryption
  - calculatePi
  - The towers of Hanoi - hanoi
- Chapter 2:
  - DNA search - dnaSearch
  - Generic search (library - run `cabal test` to execute the simple examples from the book as unit tests)
  - maze
  - Missionaries and cannibals - missionaries
- Chapter 3
  - CSP framework (library)
  - mapColoring example
  - 8 Queens example - queens
  - Word puzzle - wordSearch
  - SEND + MORE = MONEY example - sendMoreMoney
- Chapter 4
  - Graph framework (library)
  - Find the shortest path - run `cabal test` to execute the example as unit test of the graph framework.
  - Minimize cost / minimum spanning tree - run `cabal run mst`
  - Find shortest path with weighted graph - dijkstra
- Chapter 5
  - General genetic algorithm
  - SimpleEquation as simple test
  - SEND + MORE = MONEY example - sendMoreMoney
  - ListCompression
- Chapter 6
  - General KMeans algorithm (library)
  - Governors example
  - (Michael Jackson) Albums example
- Chapter 7
  - Simple neural networks framework (library)
  - Classification example: classical Iris data
  - Classification exmample: Wine data
- Chapter 8
  - Adverarial search for board games (library)
  - TicTacToe example
  - ConnectFour example

In progress:

- Chapter 9
  - Knapsack problem

Todo:

- Chapter 9
  - Traveling salesman
  - Phone number mnemonics






