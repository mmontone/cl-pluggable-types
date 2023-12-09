## Relational TypeChecker

NOT IMPLEMENTED

### Implementation idea

- Encode the types as Minikanren expressions.
- Implement a supertypeo relationship, probably using the list of supertypes for a type: https://stackoverflow.com/questions/43565851/in-common-lisp-what-is-a-function-that-returns-the-most-specific-supertype-of-t 
  supertypeo x y = y member of list-of-supertypes(x)
- Parametric type vars are encoded as logic var.
- Encode the any/dyn/unknown type encoded as logic var?
- How to report errors?
