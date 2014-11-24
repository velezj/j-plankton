j-plankton
==========

common lisp utilities 

I particular, this repository contains several ASDF systems

* j-planktin.utils
* j-plankton.concepts
* j-plankton.documentation

overview
========

In general, j-plankton.utils contains simple utilities used by most everything I write in common lisp.

Second, j-plankton.concepts defines a "concept" as well as the ability to define implementations for concepts using implementation tags (symbols).  This allows for multiple implementations of hte same thing to live in parrlalle, have specific documentation for each (important for me!), and easily changed which is currently being used

Lastly, j-plankton.documentation imcludes defsection macro which allows for documenting code in common lisp for creating simple HTML/Markdown etc. docuements which live within the code itself (and are in fact code)
