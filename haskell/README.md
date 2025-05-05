# L1C.hs

A toy compiler for a subset of the C0 language.
This is some starter code for students taking the Compiler Design class at KIT in the summer term 2025.
If you are not all that familiar with the Haskell language, the `Documentation` section below contains a little walkthrough of the codebase.

## SSA translation & IR

For now, this compiler translates the representation of the AST directly into assembly code after the semantic analysis phase.
When you get to the point where you have to come up with a format for your intermediate representation, we recommend checking out projects such as [libFirm](https://libfirm.github.io/) and [Sea-of-Nodes](https://github.com/SeaOfNodes/).
Sticking to a battle-tested approach can save you a lot of headaches later on.

In the first lab, you don't need to understand SSA in full detail.
However, register allocation on chordal graphs depends on SSA.
For Lab 1, register allocation can also be done just using the AST, but that means you'll likely have to rewrite more code in future labs.
It can still make sense to start with simple, naive implementations to have something working early on.

For an example implementation of the SSA translation and intermediate representation, you can check out the `java` branch of the starter code.

# Documentation
If you are not all that familiar with the Haskell language, here's a little walkthrough of the starter code:

## Cabal
[Cabal](https://www.haskell.org/cabal/) is a build tool for Haskell.

The `l1c.cabal` file contains all the build information for the current project.
If you want to import a new package from [Hackage](https://hackage.haskell.org/) (the Haskell package repository), just add it to the `build-depends` section of the `l1c` executable.
The `other-modules` section in the cabal file is for listing your own modules from the executable, if you don't do this cabal will yell at you at some point.

##  Important Files
The `app` directory contains the main source code of the executable.
In `l1c.cabal` we set `Main.hs` as the main file for `l1c`, and like in most other languages our main function is called `main`.

## The IO Monad
Functions that interact with the outside world (e.g. reading input, printing a line to stdout, reading a file from the filesystem, ...) are not considered "pure" in Haskell.
To make this work with Haskell's functional approach, functions like these are usually wrapped inside the `IO` monad.
This also applies to the `main` function, which has to be of type `IO ()`.
There are many good explanations of this online, for example [this one](https://www.haskell.org/tutorial/io.html) from "A Gentle Introduction to Haskell".
For some intuition on (applicative) functors and monads see [this blog post](https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html) and also consider skimming the [documentation](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Monad.html) of the `Control.Monad` package since it contains some very useful functions.

## Parsing Command Line Options
This project uses [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative) to parse command line options, the code for this can be found in the `Args.hs` file.
The `jobP` function builds a parser for a compile job containing an input file and an output file.
The `jobParser` function annotates that parser with some additional information.
In `main` we can then just pass our `jobParser` to the `execParser` function from `optparse-applicative` and it will return a `Job` result with everything else being taken care of for us.
The file also contains a little helper function that checks if the input file actually exists. Notice how this also happens inside an `IO` context (inside yet another monad, but we'll get to that in the next section).

## Error Handling
We can also make use of the power of monads to make our error handling a lot less verbose.
I recommend taking a look at [the documentation](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Except.html) of the `Control.Monad.Except` package.
This basically wraps an `Either` type, which can contain either a `Left` (which signals an error) or a `Right` (which means everything went well).
You can think of this like a `Maybe` monad (or a `std::optional` in C++) which provides some additional context in the case of failure instead of simply `Nothing`.

In `Error.hs` we first define a custom Error type called `L1Error` and some predefined exit codes for the auto-grading system.
Then we make use of our custom error type to construct an `ExceptT` of our error. This is not only a monad, but a so-called "monad transformer" (hence the T at the end), which allows us to "stack" the functionality of one monad on top of another. To get a good intuition for this you can take a look at [this article](https://en.wikibooks.org/wiki/Haskell/Monad_transformers).
Since a lot of our computation still happens inside an `IO` context, we wrap our `IO` actions inside a `L1ExceptT`, which allows us to perform side-effects but also fail in case something goes wrong.

## Compiling
Now that our command line arguments are parsed properly and we have some nice error handling set up, we can get to actually reading and compiling the provided source file.
All of this happens in the `Compile.hs` file with the individual stages in a separate subdirectory called (surprise) `Compile`.

### The Abstract Syntax Tree
The `Compile/AST.hs` file contains a simple representation of the abstract syntax tree of an L1 program as an algebraic data type (ADT).
Refer to [the Haskell wiki](https://wiki.haskell.org/index.php?title=Algebraic_data_type) for a quick refresher on ADTs.
This allows us to pattern match on the different statements(, expressions, ...) of our language later in the compilation process.
We also define some helper functions to pretty-print the different language constructs for debugging purposes. (The `Show` typeclass in Haskell basically means "has a toString function", which is fittingly named `show`).

### Parsing
Working in a functional language like Haskell, we can parse our input file with something called a "parser combinator".
[https://en.wikibooks.org/wiki/Haskell/Monad_transformersThis video](https://www.youtube.com/watch?v=dDtZLm7HIJs) gives a simple introduction to this technique.
Since we don't want to write all the different combinators ourselves, we use the [`megaparsec`](https://hackage.haskell.org/package/megaparsec) package, which provides parser combinators we can use to build our own lexer and parser for L1.
`Megaparsec` also allows us to drag around a `SourcePos` object, that we can use to provide some better location information, for example in the case of a parsing error.
We also can conveniently collect the outputs of our parser directly in an ADT.

The `parseAST` function takes a `FilePath` (which is just an alias for `String`) and returns an `L1ExceptT AST`. This means in case everything works well, we are left with a `Right` of `IO AST` which we can then pass to the next steps in our compiler and if we fail, we get a `Left` of `L1Error` which we can catch and handle accordingly.
If you now take another look at the `Compile.hs` file, you will notice that we don't match on the result of `parseAST`, but just bind it with `<-` like usual. Since the `compile` function also returns an `L1ExceptT` (this time of type unit or `()`), any exceptions thrown by us "bubble" up to the call to `compile` in `main`, where we can match once and catch every error thrown along the way. (The calls to `liftIO` are needed since `readFile` returns an `IO` value, and `liftIO` threads this through our `ExceptT` so everything works out.)

We pass our `astParser` to the `parse` function provided by `megaparsec`. Here we match on the result of the call to make use of a little helper function from `megaparsec` that allows us to display parsing errors a lot better.

The `astParser` parse a program according to the grammar described in the lab1 pdf.
The program starts with a literal `int main()`. The combinators `parens` and `braces` are a great example of the fun stuff we can do with parser combinators (in this case the `between` combinator).

We also use the `makeExprParser` function from [`Control.Monad.Combinators.Expr`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Monad-Combinators-Expr.html) which looks convoluted but is a very convenient way to build a parser for expressions with precedence.

### Semantic Analysis
We perform some very basic checks on the parsed AST:
- We can declare variables only once
- If we have already initialized a variable we cannot initialize it again
- We can only assign to variables that are already declared (or initialized)
- The program needs to contain at least one `return` statement.

To make these checks easier, we want to keep track of a table containing the variables that have already been declared and their state (for now either `Declared` or `Initialized`) while walking around our AST.
Since we don't want global state in our nice functional program, we have two options:

1. Carry this map around manually, pass it to every function, make some modifications, and return it
2. Monads (who would have thought).

This is a perfect use case for the so-called `State` monad, which does exactly what the name suggests: it lets us keep track of some additional state (in this case our variable table), without having to pass it around explicitly everywhere.
You can take a look at [this article](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State) for an introduction to the `State` monad.

Since we still want to be able to signal an error during the semantic analysis, we don't use a standalone `State` monad, but instead stack the `StateT` monad transformer on top of our `ExceptT` transformer (on top of an `IO` monad).
The article linked above in the error handling part also has a section on `StateT`.
We have used `liftIO` before, here we need to make use of the more general `lift` function (which lets us thread values through the different transformers of our "stack") to signal an error (in an `L1ExceptT` context) from a function in a `L1Semantic` context.

### Code Generation
The code generation in `Compile/AAsm.hs` is really simple and dumb:
since the abstract machine we are compiling for has an infinite amount of registers, we can just use a counter as our register allocator and count up by one for each new register we need.

Since only well-formed programs should make it this far in the compilation process, we no longer carry around an `ExceptT`, but just a plain `AST`.

Our code generation once again needs to keep track of some things:
- the registers we already allocated and their contents
- the next free register (here just an alias for `Integer`)
- a list of `String`s that keeps track of the emitted code.

Since we are working with a plain `AST` with no monads around it, we can use a simple `State` monad here to string this additional data along throughout the code generation process.
In the end we return the list of emitted instructions for the abstract machine.
Back in the `compile` function inside `Compile.hs`, the generated code gets dumped to whatever file the user provided as the second command line argument.
