j-plankton.documented-lambda-list
=================================

This system provides utilities for "documented-lambda" lists.  Similar
to an ordinary lambda list, documented lambda lists extend the syntax
to have a documentation string for every argument.  Documented lambda
lists include lambda lists for method specialisations and allow for
&key, &aux, &rest, &optional, &allow-other-keys.  Further, the system
provides ways to extrat fowarding information from documented lambda
lists which is usefull for macro writtin, etc.

The main workhorse of the system is %parse-documented-lambda-list (and
internal function!)  which takes a documented lambda list and parses
it into positional,named,rest, and other arguments.  However, such raw
format is unwieldly; this is whenre
normalize-documented-lambda-list comes in.  It turns every
argument into a property list which includes how to foward the
argument, how to define it in an ordinary lambda list, what type of
argument it is, what hte doc string is.  One can then simply look up
the information needed for each argument and process them at will,
easily.

Example documneted lambda list
==============================

	( (x "a number between 1 and 10)
	  (p "the probability of x")
	  &key
	  (use-histogram nil "do we use a histogram method for p(x) or not) )

As we see, each argument has a documentation string. We see that every
argument has now become a list with the last element the doc string.
In general, to transform an ordinary lambda list to a documented
lambda list, repalce every argument with a list and have the last
element be a string.


normalize-documented-lambda-list
========================================

For each arogument, the following properties are conputed:
* :var-foward

  How one would foward the argument to a function call.
  This includes using the keyword for named arguments
  using only hte symbol for specialized method arguments.
  Example ( (x "x doc") ( (y number) "y doc" ) &key (z "z doc")) :
	  x :var-foward = x
	  y :var-foward = y  ;; no specialization
	  z :var-foward = :z z ;; need the key to foward properly

* :lambda-list-foward

  How one would create an ordinary lambda list with this same argument.
  This will include any type specialization and any additional keyword
  bidins (default and supplied-p)

* :doc

  The documentation string for the argument.

* :original

  The original argument as it was in the documented lambda list

* :type

  one of :positional, :named (both &aux and &key are named), :rest, :other (for &allow-other-keys and any other things you put in the lambda form


              
