Implementation of a [pluggable](http://bracha.org/pluggable-types.pdf "Pluggable type systems") [gradual](http://ecee.colorado.edu/~siek/gradualtyping.html "Gradual typing") type system for Common Lisp.

Status: THIS IS VAPORWARE AND I DON'T KNOW WHAT I'M DOING.

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

For example, the type `list-of`, the parametric version of the LIST type, that takes the type of its elements by parameter.

It is possible to use them in normal type declarations (TYPE or FTYPE), as they get expanded to the equivalent Lisp type.

    (list-of integer) expands to list
    
The type parameters are simply ignored in the type definition:

    (deftype list-of (type)
        (declare (ignore type))
        'list)
        
Used in a top-level function type:

    (declaim (ftype (function ((list-of string)) string)
                    my-func))
        
That means that parametric types can be used in code that does not depend on and does not load the `pluggable-types` library. Parameter types won't be checked by Common Lisp type system, but they would still be useful as documentation, and useful for when a pluggable type system is used to check them properly. 
        
### Polymorphic types

Polymorphic types are specified using `all` to bind the type variables.

For example:

    (declaim (ftype (all (a b) (function ((hash-table-of a b) a) b))
                    get-hash))
                    
Like with parametric types, polymorphic type variables are expanded to the T type. That means they can be used in normal Common Lisp code without depending on the loading of a typechecking library.

For instance, the above type gets expanded to the valid Common Lisp type:

    (declaim (ftype (function ((hash-table-of t t) t) t)
                    get-hash))
                    
and `(hash-table-of t t)` expands to `hash-table`, so, the final type is:

    (declaim (ftype (function (hash-table t) t)
                    get-hash))

### Type checkers

There are two type checkers at the moment. Both incomplete and incorrect.
Both use `hu.dwim.walker` library to obtain an Abstract Syntax Tree of the Lisp code and walk it.

#### Constraints

The `constraints` type checker applies unification to resolve type variables, then generates constraints, and solves them. 

#### Bidirectional

The `bidirectional` type checker applies syntax-directed type checking.

### Compiler hooks

### Control via declarations
