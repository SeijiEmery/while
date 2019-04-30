# While

This is an implementation of the While toy language in Haskell for CMPS 203.

Tests are written using HUnit.

Note that I've technically used two languages, as I implemented HW 1 using c++ templates (with a haskell translation for reference), and this version is implemented purely in haskell. There is no code overlap between this and <https://github.com/seijiemery/arith>, and they use entirely different build systems (make vs stack).

### Build instructions:
    
    stack test

### Implementation Notes:

This is an interpreter, but does not have an interactive frontend, as parsing was not part of this assignment. See `test/WhileSpec` for testcases that prove that it works. In particular:

- Data.Map.Strict (non-lazy map) was used for the backing state / stores.
- The function signature for the eval functions is `Some-AST -> State -> Maybe Result`.

	    evalArith :: AExpr -> State -> Maybe Value
	    evalBool  :: BExpr -> State -> Maybe Bool
	    evalCmd   :: Cmd -> State -> Maybe State

	    where 
	        type Value = Integer
	        type Variable = String
	        type State = Data.map.Strict.Map Variable Value
	        (defined in src/While/Types.hs)

        data AExpr = (defined in src/While/AST/Arithmetic.hs...)
        data BExpr = (defined in src/While/AST/Boolean.hs...)
        data Cmd = (defined in src/While/AST/Commands.hs...)

### Language Feature:

It was natural for the eval functions to return a Maybe value. This led to a useful, somewhat interesting language feature:

- A program may be valid or invalid.
- A program is semantically valid iff it has no undefined variables and it does not contain any (trivial) infinite loops.
– Specifically, we can handle the most trivial kind of infinite loops, ie. a while-true loop that has no side effects / state changes: if the state produced after evaluating one cycle of the loop body 

As such all eval*** functions return Maybe, and program validity can be determined from that. Note that there is also a `isValidProgram :: Cmd -> Bool` function provided in `src/While/AST/Commands.hs` that does exactly that.

#### Alternative (polymorphism, and defining an undefined / null type)

The other option would be to create a composite / polymorphic type for values a la javascript, and define an `undefined` type that would have the following semantics:

- An undefined variable produces the `undefined` value
- Assigning `undefined` to a variable undefines it (ie. removes it from state)

I actually had this working at one point, but thought my current approach was more elegant (and has the advantage of being able to handle / test simple infinite while loops quite easily).

