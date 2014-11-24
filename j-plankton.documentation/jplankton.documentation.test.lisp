
(defpackage #:jplankton.documentation/test
  (:use #:cl))

(in-package jplankton.documentation/test)

;;=======================================================================
;; Make a new suite for string functions in the jplankton.documentation
;; system

(fiveam:def-suite string-test-suite)
(fiveam:in-suite string-test-suite)

;;=======================================================================
;; Some sample markdown text

(defparameter *markdown-samples*
  '( 
"Heading One
=================

Some bullted list:
* One
* Two
* Three
  * Three.One
  * Three.Two
* Four

Heading Two
-----------------
"
"A First Level Header
====================

A Second Level Header
---------------------

Now is the time for all good men to come to
the aid of their country. This is just a
regular paragraph.

The quick brown fox jumped over the lazy
dog's back.

### Header 3

> This is a blockquote.
> 
> This is the second paragraph in the blockquote.
>
> ## This is an H2 in a blockquote"
))
