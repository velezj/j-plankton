
What, exactly is a "concept" ? Read on....
	
Ok, so say that there is a particular computation that is *known*
(whether by a standard API, a set of test cases that define it, or
some other definition (perhaps a reference implemntation).  Now, we
would like to have several differnet implementations of the same
concept.  An we would like to choose which implementation to use
easily and at run-time (perhaps even using some form of machine
learning)

 Which brings us to the "concept".  A concept for use will be a
 generic method with a fixed input, so all implementations of a
 concept *must* take the same arguments.  However, the concept method
 delegates to an implementation method at runtime which is based on
 the value of a special variable for the concept.

 These "concept" variables are created in their own package and are
 made using the (define-concept ...) construction.  This ensure that
 there is only one concept per concept package, and that the binding
 of this concept symbol to an implementation tag determines the
 runtime method actually called.

 Implementations of concepts are done using the (implement-concept
 ....)  Macro, which takes the concept name, implementation tag, and
 documentation along with the lambda form and the body of the lambda.
 A new implementation tag in implicitly created in the concepts
 package if not already there. The documentation for the concept and
 the tag in the concept package is also updated to reflect the
 implementation.

 Most importantly, documentation is kept and updated, so we can do the
 following:
 
    (define-concept BAR (x y) "Documentation ofr BAR as generioc concept")
    .... some otehr part of the code
    (implement-concept (bar fast-impl "Doc of FAST version of bar concept)
       ( x y )
      ... something super fast here)
    ..... in hte repl
    (describe (find-concept-symbol 'bar))
    and we will see that FAST-IMPL is an implementation, along
    with it's docum entation as well as the generic concept doc.
