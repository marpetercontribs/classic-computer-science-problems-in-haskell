# Classic Computer Science Problems in Haskell

This repository contains my attempt at porting the source code of the books *Classic Computer Science Problems in Python* and *Classic Computer Science Problems in Java* by David Kopec to Haskell.

The source code is organized by chapter, treating each chapter as a package and code meant to be 'run' as an executable within that package. The executable name matches the name of the example in the book, e.g. `fib1`, `fib2` etc. as used in chapters 1 of the books.

If you installed Haskell using [GHCup](https://www.haskell.org/ghcup/), thus have cabal installed, you can compile and run the examples in a terminal by cd-ing into the corresponding chapter folder and executing `cabal run <name of the example>`, for example `cd chapter1` `cabal run fib2`.

You can also run the examples using the interactive Haskell interpreter. To do so, either cd into the corresponding chapter folder, start `ghci` and load the example using `:load <Example>`, e.g. `:load Fib2` (note that the file names start with an uppercase character), or start `ghci` in the root folder and load the example using `:load chapter<n>/<Example>`. In any case, you can execute the example by calling the `main` function after the example is loaded into the interactive interpreter. 

Code meant for reuse such as `generic_search` of chapter 2 and `csp` of chapter 3 is treated as a library.

Done:

In progress:

- Chapter 1:
  - Fibonacci series, examples Fib1, Fib2

