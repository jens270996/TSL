# TSL a time-symmetric language
A parser, interpreter and AST-generator (for self-interpretation) for the time-symmetric language TSL.

## Dependencies
The project is written in Haskell, and uses cabal for build-management.

The dependencies can be installed using
```cabal install```

## Building the project
The project can be built using
```cabal build```

## Running the project
The project can be run using the command

```cabal run -- TSL [args]```

To see which options are available, use the `-h` flag:
```
cabal run -- TSL -h
Usage: TSL COMMAND

  An interpreter for TSL

Available options:
  -h,--help                Show this help text

Available commands:
  interpret                Interpret a TSL program
  convert                  Convert a TSL program into AST used by
                           self-interpreter
  pair                     Pair two input files i1 and i2 into a single input
                           (i1.i2)
```
Now the possible commands are explained:

```cabal run -- TSL interpret <Program file> <Input file> [-v|--verbose]```
Runs the interpreter on the program in `Program file` with the input constant in `Input file`.

```
TSL convert <Program file> <Destination file> [-o|--ordered]
                   [-v|--verbose]
```
Converts the program into a TSL constant representation of the AST of `Program file`. This AST representation is used with the self-interpreter in `progs/SelfInterpreter/selfInterpreter.tsl`.

```TSL pair <Input file 1> <Input file 2> <Destination file> [-v|--verbose]```
Takes two files containing TSL constants, and writes them as a pair constant to `Destination file`. This is a utility for creating `(ProgramAST.Input)` pairs to be used with the self interpreter.

## Example programs
There are several example programs written in TSL in the `progs/` folder. The `.tsl` files contains the programs, and the `.in` files the corresponding input constants. The `progs/RTM` folder contains a involutization of a RTM-interpreter. The `progs/TSTM` contains a TSTM-interpreter using symmetrization. The `progs/SelfInterpreter` contains a TSL self-interpreter, and some AST representaions of the other programs, that can be passed to the self-interpreter. Finally, the `progs/SelfInterpreter/SelfInterpreter` contains a AST representation of the self-interpreter, that can be passed to the self-interpreter. This is used for testing the capabilites of the self-interpreter. Note that running the self-interpreter is very slow, even for smaller programs.

## Testing the project
The project uses the `Tasty` package for testing. The project is tested with unit-tests, using the `Tasty.HUnit` module.

The tests can be run using
```cabal test```
