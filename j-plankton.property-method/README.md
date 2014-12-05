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
properties of an pbject should be the most specialized.


Interna Details (the how!)
==========================

Internally, we in fact create a completely separate class hiearchy for
hte properties existing in an object. We then use the normal common
lisp generic dispatch and method specialization to pick the right
method (and next method) based on *both* this properties class and the
actual object class/type.
