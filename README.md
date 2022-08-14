# Compiler

This project attempts to create a custom language and a compiler for it that generates LLVM IR.

~~A few examples of the language can be found in `fib.idk`, `fibR.idk`, `test.idk`, and `test.old.idk`.~~


This project used to be focused on generating MIPS code, but now I've transitioned to trying to use LLVM to simplify things.

Currently the language features functions, recursion, if, else, and while statements. The only type currently supported is integers.

See `comparison/README.md` for speed comparisons. Currently it looks to be 35% slower than C.

## How to run

Run `make build` to compile the compiler.

Run
```bash
cabal run compiler -- -i fibR.idk
```
to compile the `fibR.idk`. Then execute `./a.out` to run the compiled code.
<!-- 
To compiler `fibR.idk` for example (fibonacci numbers with recursion), run 

`./Main -i fibR.idk -o fibR.s`

To run the generated MIPS assembly, I've used the MIPS32 simulator `spim`. Use `spim -f fibR.s` to run.

To run the automatic tests, run `pytest`. (This requires `python3` and `pytest`).
 -->

## ToDo List

- [ ] Enums on the heap
