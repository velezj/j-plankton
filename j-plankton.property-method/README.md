j-plankton.property-method
=================================

This system provides the ability to specify methods based on the
existance of "properties" contained in an object.  Properties are
orthogonal to class/type and do not represent "is-a"
relationships. Rather, a property is a tag which can be added to an
object at any time, with a set value, and whoose existance in the
object denotes some salient condition on the algorithm/method to use.
Furthermore, properties have subs-set behaviour and this system allows
that.  For example: an object with properties :a :b :c and another
object with properties :a :c :d both are objects with porperties :a
:b.  Similarly, specializing a method for objects with :a :b
properties should allow to be a valid method for eather object.  That
being said, methods specialing the most (in terms of number) of
properties of an object should be the most specialized.



Property Inheritance
====================

Just like the regular class of an object has a sence of a precedence list, the properties of an object *alos* have a sense of a precendence list.  Here, we ensure that subsets of properties are parents (super-types) of sets of properties.  Here is an example:

     * props1 = a,b   ;; two properties named a,b
     * props2 = a,c
     * props3 = a,b,c

Here we have three objects with different properties.  We want the following:

(a) -> (a b) -> (a b c)

So, the probs3 should have a superclass of props1, and eventually of (a), (b), and (c) top-level superclasses for individual properties.  This is a subset-is-superclass we ensure.

What does thie means


Internal Details (the how!)
==========================

Internally, we in fact create a completely separate class hiearchy for
hte properties existing in an object. We then use the normal common
lisp generic dispatch and method specialization to pick the right
method (and next method) based on *both* this properties class and the
actual object class/type.
