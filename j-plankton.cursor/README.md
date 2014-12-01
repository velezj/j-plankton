j-plankton.cursor
=====================

This system supplies a rotocol for iteration based on cursors.
Also, it provides useful utilities to merge/transform cursors.

The protocol is as follows:

	* (hash-next-p <cursor>) -> generalized boolean
	* (value <cursor>) -> T, the current value for the cursor
	* (next <cursor>) -> T, increments cursor and returns this new value
	* (reset <cursor>) -> (), resets the cursor to start from the beginning

Calling (next...) when there are no elements raises a
CURSOR-FINISHED-CONDITION condition.


