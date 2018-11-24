# Map Coloring as a SAT Problem

I wrote a little bit of Haskell in and for my
[Spring 2010 CS 543](http://wiki.cs.pdx.edu/cs543-spring2010)
class to demonstrate the power of solving search problems
by transforming them into SAT problems and hitting them with
a SAT solver. The Git repo contains the Haskell source for
a map coloring encoder.

The encoder takes as input (on stdin) a file containing
a list of regions.  Each region is a line of the form
    index color adjacency adjacency ...
The indices must start at 1 and count up.  The color should
be a single character.  The character '?' is interpreted specially
to mean an unknown color.  The adjacencies are just the indices
of adjacent regions.  Normally these would be symmetric.

The number of colors to be used is an optional argument to the
encoder.  The default is 4.

The output of the program (on stdout) is a SAT problem in
[DIMACS format](http://www.satlib.org/Benchmarks/SAT/satformat.ps)
suitable for feeding to a SAT solver such as [MiniSat2](http://minisat.se/MiniSat.html).

A sample map produced by the _map_ puzzle from [sgt-puzzles](http://www.chiark.greenend.org.uk/~sgtatham/puzzles/) is
included in the distribution for test purposes,
along with a variant that has two non-adjacent
regions connected to be unsat.  Both were hand-encoded.  It would be
great if someone wrote an auto-encoder for the sgt-puzzles map generator.

The code is licensed under the "MIT license".  Please see
the file `COPYING` in this distribution for license terms.
