j-plankton.cursor
=====================

This system supplies a rotocol for iteration based on cursors.
Also, it provides useful utilities to merge/transform cursors.

The protocol is as follows:

	* (done-p <cursor>) -> generalized boolean
	* (value <cursor>) -> T, the current value for the cursor
	* (next <cursor>) -> T, increments cursor and returns this new value
	* (reset <cursor>) -> (), resets the cursor to start from the beginning
	* (clone <cursor>) -> <cursor copy>, where it is guaranteed that calling
	    (reset <copy>), (next <copy>) will not change the original cursor
		however, the element returned may share structure

Calling (next...) when there are no elements raises a
CURSOR-FINISHED-CONDITION condition.


Utilities
=========

In general, we provide several *Creators*, *transformers* which create
new cursors from given cursors, *sweeps* which step a ste of cursors
in a predefined manner, and lastly the ability to *materialize* a
cursor which menas returning a sequence with the elements of the
cursor.

transformers
------------

The most powerful utility for cursors is a tranformer.  Here, we take
a set of cursors and create a new cursor from applying a user-function
to the values of the cursors set.  The (next ....) of a tranformed
cursor calls (next ...) on hte cursor set it contains.  To determine
whether a transformer cursor is done we have two choices:

	:firt termination means that the transfomer cursor stops when the
    first of the cursor set stops

	:last termination means that the tranformer cursor stops when the
	last of the cursor set stops. For this option we need to define
	default values per-cursor to use when they no longer have elements
	but other in the set do


