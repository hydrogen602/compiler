# Compiler

~~This project attempts to create a custom language and a compiler for it that generates MIPS assembly.~~

~~A few examples of the language can be found in `fib.idk`, `fibR.idk`, `test.idk`, and `test.old.idk`.~~

This project used to be focused on generating MIPS code, but now I've transitioned to trying to use LLVM to simplify things.

## How to run

Run `make` to compile & run the compiler, compile and link the `.idk` file and run it.
<!-- 
To compiler `fibR.idk` for example (fibonacci numbers with recursion), run 

`./Main -i fibR.idk -o fibR.s`

To run the generated MIPS assembly, I've used the MIPS32 simulator `spim`. Use `spim -f fibR.s` to run.

To run the automatic tests, run `pytest`. (This requires `python3` and `pytest`).
 -->
