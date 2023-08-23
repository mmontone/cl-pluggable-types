Implementation of a [pluggable](http://bracha.org/pluggable-types.pdf "Pluggable type systems") [gradual](http://ecee.colorado.edu/~siek/gradualtyping.html "Gradual typing") type system for Common Lisp.

Status: THIS IS VAPORWARE [AND](http://www.lispworks.com/reference/HyperSpec/Body/a_and.htm) I DON'[T](http://www.lispworks.com/reference/HyperSpec/Body/a_t.htm) KNOW WHAT I'M DOING.

## Papers

* [Gradual Typing for Functional Languages](http://www.cs.colorado.edu/~siek/pubs/pubs/2006/siek06:_gradual.pdf)
* [Pluggable type systems](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.175.1460)
* [Static typing where possible, dynamic typing when needed: the end of the cold war between programming languages](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.69.5966&rep=rep1&type=pdf)
* [Typed Scheme: From Scripts to Programs](http://www.ccs.neu.edu/racket/pubs/dissertation-tobin-hochstadt.pdf)
* [Languages as Libraries](http://www.cs.utah.edu/plt/publications/pldi11-tscff.pdf)
* [Gradual typing for Smalltalk](http://pleiad.cl/research/publications?key=allendeAl-scp2013)
* [Extending Dylan type system for better type inference and error detection](https://www.researchgate.net/publication/228771491_Extending_Dylan's_type_system_for_better_type_inference_and_error_detection)

## Other implementations

* [An optional type system for Clojure](https://github.com/clojure/core.typed)
* [Typed Racket](http://docs.racket-lang.org/ts-guide/)
* [GradualTalk: A Practical Gradual Type System For Smalltalk](http://pleiad.cl/research/software/gradualtalk)

## Plan

1. Functions typespecs (arity, optional and keyword args, etc)
2. Add type specs to common-lisp package functions
3. Try to type a big module (replace :use :cl by :use :gradual)
4. Tests
5. CLOS, structs
6. Polymorphism
7. Inference

## Current implementation

Here I describe my current approach for an implementation.

### Parametric types

A library of parametric versions of Common Lisp types. The types fallback to 'normal' CL types, so they can be imported and used with no dependency on the pluggable type system.

For example, the type `list-of`, the parametric version of the [LIST](http://www.lispworks.com/reference/HyperSpec/Body/a_list.htm) type, that takes the type of its elements by parameter.

It is possible to use them in normal type declarations ([TYPE](http://www.lispworks.com/reference/HyperSpec/Body/a_type.htm) or [FTYPE](http://www.lispworks.com/reference/HyperSpec/Body/d_ftype.htm)), as they get expanded to the equivalent Lisp type.

    (list-of integer) expands to list
    
The type parameters are simply ignored in the type definition:

    (deftype list-of (type)
        (declare (ignore type))
        'list)
        
### Polymorphic types

### Typechecking backends

#### Constraints

#### Bidirectional

### Compiler hooks

### Control via declarations
