This is an interpreter for the [Brainf*** Language](https://en.wikipedia.org/wiki/Brainfuck) (because of my dislike of swearing, I use a censored version of the name) written in SWI Prolog (tested with SWI Prolog Version 7.2.3 so using Prolog Strings instead of lists of atom codes). 

The source code was originally completed as an assignment for the [Advanced Logical Programming Course](http://stups.hhu.de/w/Logische_Programmierung_2,_SoSe_15) at the HHU in Duesseldorf, Germany.

## How the language works

The language works by saving integer values on an infinitely large band (modelled as an array in many implementations). There are eight commands, each symbolized by a single character. In this prolog implementation, we have implemented the band using the `-` functor so that the band has the following form `Left-Current-Right`, where `Left` and `Right` are prolog lists and `Current` is a number. The behavior of the commands are therefore as follows:

| **Character** | **What It Does**                     |
| ------------- | ------------------------------------ |
| `>`           | Moves band one position to the right<br> (i.e. `L-E-[H|T] --> [E|L]-H-T`, or `L-E-[] --> [E|L]-0-[]` if no space to the right exists yet) |
| `<`           | Moves band one position to the left<br> (i.e. `[H|T]-E-R --> T-E-[H|R]`, or `[]-E-R --> []-0-[E|R]` if no space to the left exists yet) |
| `+`           | Increment the value at the current positon on the band (i.e. `L-E-R --> L-E2-R, E2 is E + 1`) |
| `-`           | Decrement the value at the current position on the band (i.e. `L-E-R --> L-E2-R, E2 is E - 1`) |
| `.`           | Write the current value `_-E-_` to stdout |
| `,`           | Retrieve a character from stdin and place it on the band |
| `[`           | If the value on the band is 0, jump the instruction pointer to the command after the matching `]` |
| `]`           | If the value of the band is nonzero, jump the instruction pointer back to the matching `[` |

The typical way to interpret the program is to work on the string directly and use a stack to keep track and be able to find the correct entry parenthesis when having reached an end parenthesis.

But this is Prolog, where it is ridiculously easy to write parsers with a GCD, so I went ahead and wrote quick parser and create an abstract syntax tree where the instructions that come between two parenthesis are grouped together in a `loop(Instructions)` clause. When this instruction is interpreted, if the band value is 0, the loop is ignored and the interpreter automatically moves to the instruction after the loop. Otherwise, the `Instructions` are run through the interpreter again and then `loop(Instructions)` is called recursively with the new band and input and output streams.

There are two different interpret functions which are available in `interpreter.pl`. One uses strings (or more exactly lists of atom codes) to simulate stdin and stdout. The other writes directly to the real stdout and reads directly from stdin. The first version is used for the unit tests which test the interpreter. `brainf.pl` defines a main predicate so that the interpreter can be called directly from the command line. This takes one argument, the filename of the Brainf*** program:

	swipl -l brainf.pl -- helloworld.bf
	
The input and output streams can be redirected into the program as well:

	%% This will print the helloworld.bf program on the console
	swipl -l brainf.pl -- cat.bf < helloworld.bf
	
