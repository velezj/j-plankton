j-plankton
==========

common lisp utilities 

I particular, this repository contains several ASDF systems

* j-planktin.utils
* j-plankton.documented-lambda-list
* j-plankton.concepts
* j-plankton.documentation

overview
========

In general, j-plankton.utils contains simple utilities used by most everything I write in common lisp.

Second, j-plankton.concepts defines a "concept" as well as the ability to define implementations for concepts using implementation tags (symbols).  This allows for multiple implementations of hte same thing to live in parrlalle, have specific documentation for each (important for me!), and easily changed which is currently being used.

The concepts defined inside j-plankton.concept all use "documented lambda lists" which are augmented lambda lists where *every* argument has a documentation string.  Functions to parse, foward, and query such documented lambda lsits are in the j-plankton.documented-lambda-list system.

Lastly, j-plankton.documentation imcludes defsection macro which allows for documenting code in common lisp for creating simple HTML/Markdown etc. docuements which live within the code itself (and are in fact code)
