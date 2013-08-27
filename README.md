Implementation of a [pluggable](http://bracha.org/pluggable-types.pdf "Pluggable type systems") [gradual](http://ecee.colorado.edu/~siek/gradualtyping.html "Gradual typing") type system for Common Lisp.

Status: THIS IS VAPORWARE AND I DON'T KNOW WHAT I'M DOING.

Resources:

* [An optional type system for Clojure](https://github.com/clojure/core.typed)
* [Typed Racket](http://docs.racket-lang.org/ts-guide/)

Plan:

1. Functions typespecs (arity, optional and keyword args, etc)
2. Add type specs to common-lisp package functions
3. Try to type a big module (replace :use :cl by :use :gradual)
4. Tests
5. CLOS, structs
6. Polymorphism
7. Inference