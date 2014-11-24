j-plankton.documentation
========================

This is a simple system to define documentation right into your common
lisp code.  This is mostly for pages of documentation that are not
nessesarily API docs for methods, but more general overview pages,
usage tips, etc.

Documentation in this way is defined in terms of "sections", each of
which contains a set of "blocks" of documentation.  The format of the
documentation is pecified *per-block* and defaults to markdown format.
More importantly, documentation of other sections defined can be
included ina section (thereby mking sub-sections!).  Once a sectio has
been defined (using (defsection)) then one can convert a section into
a full documentation with a given format (say HTML, Mardown, latex,
etc...) using (materialize-documentation).

One of the best features of defsection is that blocks may have
completely different formats when written, so one can freely mix Latex
formatted documentation with Mardown.  Each block type has a concept
of whether they require leading/trailing newlines when materializing.

defsection
==========

The defsection macro is the starting point and only macr a user needs to create documentation.  The simple usage is as follows:

    (defsection (<name> <title>)
		<block 0>
		<block 1>
		...
		)

<name>: a symbol, usually preffixed by '@', such as @intro.  This
symbol will be used as the section id by first calling (labelize-name)
on it's string representation.  The symbol will be globally bound with
the section object defined by defection.

<title> : a string represneting a title/subtitle section heading

<block N> : a block descriptor, documentated below

After a call to (defsection ...) we will have created a new global
binding to the given symbol <name> with a section-t object.  To
generate the codumentation for a section, we can the ncall
(materialize-documentation <name>)


blocks
======

The heart of defsection are the <block N> definitions inside of it.  A
block definition is a user-extendible syntax for creating different
logical chuncks of documetatation, where the logical break includes
the format being used to initially represent the documentation.

A string by itself will be treated as a text block with Mardown format.

Alternatively, one can expresilly choose hte format for a text block
by having a block descriptor as:

	(:<format> <text>)

Where :<format> is a keyword format specifier such as :markdown, :latex, etc...

To creat rbritary types of blocks we use the following syntax:

	(:class <class-name> <class-args>)

Which will fowards <class-args> to (make-instance '<class-name> <class-args>)


Example
=======

	(j-plankton.documentation:defsection (@impl-note "IMPL Note")
		"We are using the algorithm by Jorkas, which implies the following condition: "
		(:latex "$x_i \le Z^i_k \forall k$ iff. ...."))

	(j-plankton.documentation:defsection (@intro "Intro")
		"Some sample introduction using *Mardown*")

	(j-plankton.documentation:defsection (@doc "Documentation for X")
		@intro
		@impl-note)


