# Compiler

This project attempts to create a custom language and a compiler for it that generates MIPS assembly.

A few examples of the language can be found in `fib.idk`, `fibR.idk`, `test.idk`, and `test.old.idk`.

## How to run

Run `make` to compile the compiler

To compiler `fibR.idk` for example (fibonacci numbers with recursion), run 

`./Main -i fibR.idk -o fibR.s`

To run the generated MIPS assembly, I've used the MIPS32 simulator `spim`. Use `spim -f fibR.s` to run.

To run the automatic tests, run `pytest`. (This requires `python3` and `pytest`).

