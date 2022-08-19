# General

More functional
Prefer expressions over statements

Functional C?

# Types

The usual: i32, f64, etc.
- void is `()`, a zero-byte thing
- blob type for some data

Generic Type Params: Rust Style? -> would be nice to not have to specify

Protocols/Typeclasses

Kinda want types to be arbitrary, i.e. an i32 is not instrinsically a number, it just happens to have a binary operation called addition.

# Syntatic Sugar

```
x.foo(y)
```
is turned into
```
TypeName_foo(x, y)
```
where `TypeName` is replaced with the name of the type of `x`, i.e. if `ls` is of type `ArrayList`, then `ls.pop()` is `ArrayList_pop(ls)`

# Memory handling

Reference counting

Every heap-allocated struct should also come with a function of the name TypeName_destroy, i.e. `ArrayList_destroy` that decrements the reference counter and frees all memory if `rc_decr` returns `NO_LONGER_IN_USE`.

- Whenever a scope goes away, i.e. a block is exited, all variables of that scope that are heap-allocated have `rc_decr` called on them. 
- When to increment
  - When a value is assigned to a new variable, i.e. `let y = x`
  - When a value is passed to a function, i.e. `foo(x)`
  - When a value is being returned from a function, i.e. `return x`
  - When a value is being passed out of an if block, i.e. `if condition { x } else { y }`

