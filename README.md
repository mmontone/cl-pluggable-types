Implementation of a [pluggable](http://bracha.org/pluggable-types.pdf "Pluggable type systems") [gradual](http://ecee.colorado.edu/~siek/gradualtyping.html "Gradual typing") type system for Common Lisp.

Status: THIS IS VAPORWARE AND I DON'T KNOW WHAT I'M DOING.

Papers:

* [Gradual Typing for Functional Languages](http://www.cs.colorado.edu/~siek/pubs/pubs/2006/siek06:_gradual.pdf)
* [Pluggable type systems](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.175.1460)
* [Static typing where possible, dynamic typing when needed: the end of the cold war between programming languages](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.69.5966&rep=rep1&type=pdf)
* [Typed Scheme: From Scripts to Programs](http://www.ccs.neu.edu/racket/pubs/dissertation-tobin-hochstadt.pdf)
* [Languages as Libraries](http://www.cs.utah.edu/plt/publications/pldi11-tscff.pdf)
* [Gradual typing for Smalltalk](http://pleiad.cl/research/publications?key=allendeAl-scp2013)
* [Extending Dylan type system for better type inference and error detection](https://www.researchgate.net/publication/228771491_Extending_Dylan's_type_system_for_better_type_inference_and_error_detection)

Other implementations:

* [An optional type system for Clojure](https://github.com/clojure/core.typed)
* [Typed Racket](http://docs.racket-lang.org/ts-guide/)
* [GradualTalk: A Practical Gradual Type System For Smalltalk](http://pleiad.cl/research/software/gradualtalk)

Plan:

1. Functions typespecs (arity, optional and keyword args, etc)
2. Add type specs to common-lisp package functions
3. Try to type a big module (replace :use :cl by :use :gradual)
4. Tests
5. CLOS, structs
6. Polymorphism
7. Inference
