# Description
A messy but functional implementation of the simply typed lambda calculus
written mostly for learning purposes.
Running reduce.sh on a file containing a lambda expression will print the
beta-reduced equivalent.
The interpreter uses a form of lazy evaluation to stop unnecessesary
computation being performed.
I'm not quite sure if it's optimal in that regard but it should be better
than strict evaluation.

# Dependencies
+ [OCaml](https://ocaml.org/)
+ [Dune](https://dune.build/)

# Installation
1. Clone this repository
2. Run `dune build`
3. Running `./reduce.sh example.src` should print `s` and `example2.src` `true`

# Syntax
The [simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus)
is essentially the regular lambda calculus but all variables are associated
with a type.
As the name would suggest the types are extremely simple, they can be either a
base type such as `string` or a function type such as `string -> int` which
takes in an expression with type `string` and produces and expression with
type `int`.
For example the string identity function would be written as `\s : string. s`
and would be applied like `(\s : string. s) (greeting : string)`.
As you can see to facilitate writting lambda expressions we use `\` instead of
`λ`(the actual lambda symbol).

# Why?
I'm really interested in language design and this kind of project seemed like a
good way to gain experience in implementating programming languages as it's
quite a small project so any mistakes I make won't seriously cost me any time.
This project especially was meant to help me learn about implementing type
systems.

# Why is it messy?
I was really excited to get this working after finishing [my implementation] of
the regular lambda calculus so I rushed through writting the code.
This, as you might expect, resulted in most of the code (especially the parser)
being a complete mess.
I'm planning to comletely redo this entire project now knowing everything I learned
from building this.

# Why OCaml?
1. Pattern matching is awesome for this kind of thing
2. Go doesn't have good support for union types (if it did I would probably be
using that as it's subjectively the best programming language yet)
3. https://www.cs.ubc.ca/~murphyk/Software/Ocaml/why_ocaml.html
