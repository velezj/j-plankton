;;;; jplankton.documentation.lisp

(in-package #:j-plankton.documentation)

;========================================================================
;; A text-t is a string and an encoding together
(defclass text-t ()
  ((encoding :accessor text-encoding
	     :initarg :encoding
	     :initform :utf-8)
   (format :accessor text-format
	   :initarg :format
	   :initform :markdown)
   (text-string :accessor text-string
		:initarg :text)))
;========================================================================
;; make a text object from a string and encoding (default to :utf-8)	
(defun make-text (string &key (format :markdown) (encoding :utf-8))
  (make-instance 'text-t
		 :text string
		 :format format
		 :encoding encoding))

;========================================================================
;; pretty print text-t objects
(defmethod print-object ( (text text-t) stream )
  (print-unreadable-object (text stream :type t :identity nil)
    (princ (text-string text) stream)
    (princ " (" stream)
    (princ (text-format text) stream)
    (princ "/" stream)
    (princ (text-encoding text) stream)
    (princ ")" stream)))
	 

;========================================================================
;; A section is an identified block of text with a label for linking
;; It is also a "section" as per a document, which means a logical
;; grouping with a title of some form
(defclass section-t ()
  ((id :reader id
       :initarg :id)
   (label :reader label
	  :initarg :label)
   (title :accessor title
	  :initarg :title)
   (blocks
    :accessor blocks
    :initarg :blocks
    :initform '())))

;========================================================================
;; pretty print section-t objects
(defmethod print-object ( (section section-t) stream)
  (print-unreadable-object (section stream :type t :identity nil)
    (princ (id section) stream)
    (when (not (eq (id section) (label section)))
      (princ "[" stream)
      (princ (label section) stream)
      (princ "]" stream))
    (dolist (blk (blocks section))
      (terpri stream)
      (princ "  " stream)
      (if (and (eql 'text-t (type-of blk)) (< (length (text-string blk)) 50))
	  (princ blk stream)
	  (print-unreadable-object (blk stream :type t :identity t))))))

;========================================================================
;; A global registry of sections mapped by id
(defparameter *sections-by-id* (make-hash-table)
  "A hashtable mapping id symbol to section object")

;========================================================================
;; A package for section label and symbols
(defpackage #:jplankton.documentation/labels)
(defparameter *labels-package* (find-package :jplankton.documentation/labels))

;========================================================================
;; Define a new section of documentation iwth a given identifier.
;; Usually, ids begin with the '@' sign to denote these as sections.
;; The provided block blk must be ablock-dscriptor or nil
;; You can optionally specify a label for the section to link.
(defun %make-section (title &key (label nil) (id (gensym "Gsec")))
  (let* ((id-symbol 
	   (ctypecase id 
	     (symbol id)
	     (string (intern id *labels-package*))))
	 (label-symbol
	   (ctypecase label
	     (null id-symbol)
	     (symbol label)
	     (string (intern label *labels-package*))))
	 (blocks (list))
	 (section
	  (make-instance 'section-t
			 :title (or title "")
			 :id id-symbol 
			 :label label-symbol 
			 :blocks blocks)))
    ;; register the section
    (let ((prev (gethash id-symbol *sections-by-id* nil)))
      (when prev
	(error "Section id's must be unique! cannot make new section ~S with id ~S, already registered that id with section ~S."
	       section
	       id-symbol
	       prev)))
    (setf (gethash id-symbol *sections-by-id*)
	  section)
    section))
	    

;;========================================================================
;; create a new block from a block-descriptor
;; This is a generic method specialized for different
;; descriptors
;;
;; In particular, descriptors which are lsits beginning
;; with a keyword are further specified using
;; the make-block-by-keyword gneric method
(defgeneric make-block (block-descriptor)
  (:documentation "generates a block object given a description"))


;;========================================================================
;; Craete a new block given a block keyword and the rest of the
;; block descriptor
(defgeneric make-block-by-keyword (kw descriptor)
  (:documentation "generates a block given a keyword and descriptor"))

;;========================================================================
;; specify make-block for string descriptors.
;; A plain string block descriptor is craeted into a text-t block
;; and first processed using %strip-whitespace-smartly
(defmethod make-block ( (str string) )
  (make-text (%strip-whitespace-smartly str)))

;;========================================================================
;; specify make-block when given a symbol.
;; If the symbol has a bound value, use that value
;; as the block, otherwise try to look up the symbol in
;; the section id.
;; It is an error if the symbol is not bound or a section id
(defmethod make-block ( (s symbol) )
  (if (boundp s)
      s
      (or
       (gethash s *sections-by-id* nil)
       (error "Cannot make a block out of a symbol which is not bound or a section id, bad symbol: ~S" s))))

;;========================================================================
;; Specify make-block for block descripros which are lists
;; This particular specification will check if teh list starts with
;; a keyword and dispath the call to make-block-by-keyword
(defmethod make-block ( (list-descriptor list) )
  (let ((kw (car list-descriptor))
	(desc (if (= 2 (length list-descriptor))
		  (nth 1 list-descriptor)
		  (cdr list-descriptor))))
    (make-block-by-keyword kw desc)))

;;========================================================================
;; Specify make-block for nil descriptors (this is an error)
(defmethod make-block ( (descriptor (eql nil)))
  (error "Nil os not a valid block descriptor!"))

;;========================================================================
;; Specify a default make-block-by-keyword which assumes that the
;; keyword is a text format and assumes teh descriptor is a
;; single string, creating a new text-t block from it with the
;; given format (the keyword)
(defmethod make-block-by-keyword ( (format t) (descriptor string) )
  (make-text descriptor :format format))


;;========================================================================
;; Specify make-block-by-keyword for :class keyword.
;; The descriptor is fed directly to make-instance to
;; create a block
(defmethod make-block-by-keyword ( (kw (eql :class)) args )
  (apply #'make-instance args))

;;========================================================================
;; Specify make-block-by-keyword for :sub-section keywords
;; The descriptor is the given to defsection minus the id
;; (defmethod make-block-by-keyword ( (kw (eql :sub-section)) def-section-args)
;;   (let ((name (gensym)))
;;     (destructuring-bind ((title &key (id name) (label (labelize-name (string name)))) &body blocks) def-section-args
;;       (defsection ((symbol-value name) title
;; 		   :id id
;; 		   :label label)
;; 	blocks)
;;       (symbol-value name))))

;;========================================================================
;; A serial-block-t is just a group of blocks in order
(defclass serial-block-t ()
  ((blocks :initform nil
	   :initarg :blocks)))

;;========================================================================
;; Make a serial block from a list of blocks
(defun make-serial-block (blocks)
  (make-instance 'serial-block-t :blocks blocks))

;;========================================================================
;; specify make-block-by-keyword for :serial-block keyword
;; to make a serial block
(defmethod make-block-by-keyword ( (kw (eql :serial-block)) blocks )
  (make-serial-block blocks))

;;========================================================================
;; creates a block of documentation from a particular symbol and
;; it's common list DOCUMETNATION docs.
;; the :format keyword is a list of specifiers for how and what docs
;; are used, and can be the following (defaults to '(:fancy t :doc-type :all)):
;;    :fancy (t/nil) = do we pretty-print things in a fancy format
;;                     (defualt to t)
;;    :doc-type = what docuementation do we query. This is a list with
;;                the documentation wanted, or :all which means to use
;;                all possible documentation. The possible doc-types are:
;;                    'compiler-macro
;;                    'function
;;                    'method-combination
;;                    'setf
;;                    'structure
;;                    't
;;                    'type
;;                    'variable
;;                (defaults to :all)
(defun make-api-block (symbol &key (format '(:fancy t :doc-type :all)))
  (destructuring-bind (&key (fancy t)
			    (doc-type :all))
      format
    ;; ok, first take care of :all doc-type by adding the docs based on
    ;; the type of symbol
    (when (eql doc-type :all)
      (setf doc-type
	    (etypecase symbol
	      (function (list 't 'function))
	      (list (list 'function 'compiler-macro))
	      (symbol (list 'function 'compiler-macro 'setf 'method-combination 'type 'structure 'variable))
	      (method-combination (list 't 'method-combination))
	      (standard-method (list 't))
	      (package (list 't))
	      (standard-class (list 't 'type)))))
    ;; ok, now create an output string for the queried documentation
    (make-text
     (with-output-to-string (s)
       (if fancy
	   (format s "~A~%===============~%" symbol)
	   (format s "~A: " symbol))
       (dolist (dtype doc-type)
	 (when (documentation symbol dtype)
	   (if fancy
	       (format s "* ~A: ~%     ~A~%~%" dtype (documentation symbol dtype))
	       (format s "~A~%" (documentation symbol dtype)))))))))
       
       
;;========================================================================
;; specify make-block-by-keyword for :api keywords.
;; These blocks take any number of symbols after :symbols and
;; include the API documentation for those symbols.
;; additionally, further keywords are available:
;;    :prefix-block = attach the given block before any api block
;;                    (defaults to #\Newline)
;;    :postfix-block = attach given block after any api block
;;                     (defaults to nil)
;;    :api-format = arguments given to (make-api-block :format <>)
;;                  (defaults to (:fancy t :doc-type :all))
;;    :symbols = list of symobls to add documentation for
(defmethod make-block-by-keyword ( (kw (eql :api)) args)
  (destructuring-bind
	(&key
	 symbols
	 (prefix-block (string #\Newline))
	 (postfix-block nil)
	 (api-format '(:fancy t :doc-type :all)))
      args
    ;; create a serial block
    (make-serial-block
     (loop for s in symbols
	when (not (null prefix-block))
	collect (make-block prefix-block)
	collect (make-api-block s :format api-format)
	when (not (null postfix-block))
	collect (make-block postfix-block)))))
		       

;;========================================================================
;; Given a string, returns a string which can be used for a label.
;; This removes any special characters and removes any leading '@' sign
(defun labelize-name (name)
  (let ((removed-first-at
	 (if (and name
		  (> (length name) 0)
		  (char= (elt name 0) #\@))
	     (subseq name 1)
	     name)))
    (cl-ppcre:regex-replace-all
     "\\W+" removed-first-at "-"))) 

;;========================================================================
;; macro to defien a section with documentation an id=label
;; binds teh section to the given symbol as well (used as id/label)
(defmacro defsection ((name title &key (id name) (label (labelize-name (string name)))) &body blocks)
  (when (null blocks)
    (error "Sections must be defiend with at least 1 block!"))
  `(progn
     (defconstant ,name (%make-section ,title :label ',label :id ',id))
     (setf (blocks ,name)
	   (list (make-block ',(car blocks))))
     ,@(loop for d in (cdr blocks)
	  collect
	    `(nconc (blocks ,name)
		    (list (make-block ',d))))))
     
	  


;;========================================================================
;; returns a documentation string for a particular block
;; this is a generic method which must be implemented for any
;; blocks inside of sections
(defgeneric materialize-documentation (blk
				       &key
					 format
					 stream
					 depth
				       &allow-other-keys)
  (:documentation "materialzes the documentation for a block to the given
                   stream in the given format (usually a keyword).  If stream
                   is nil we return the documentation as a string"))

;;========================================================================
;; returns true iff the given block type needs a leading newline when
;; concatenated into a final documentation.  This controls the "inline-ness"
;; of different blocks
(defgeneric block-needs-leading-newline (blk &key format)
  (:documentation "returns ture iff the block type needs a learding newline
                   when concatenating into documentation"))
(defgeneric block-needs-trailing-newline (blk &key format)
  (:documentation "returns ture iff the block type needs a trailing newline
                   when concatenating into documentation"))

;; by default, blocks are inline, no leading or trailing newlines needed
(defmethod block-needs-leading-newline ( (blk t) &key format )
  nil)
(defmethod block-needs-trailing-newline ( (blk t) &key format )
  nil)


;;========================================================================
;; materialize documentation for a text block internally calls
;; the pandoc %translate-using-pandoc
(defmethod materialize-documentation ( (text text-t)
				      &key
					(format :latex)
					(stream nil)
					(depth nil))
  (let ((doc (%translate-using-pandoc
	      :input-utf-8 (text-string text)
	      :input-format (text-format text)
	      :output-format format)))
    (format stream "~A" doc)))


;;========================================================================
;; materialize documentation for a serial-block-t.
;; This just call materialize-documentation for each internal block
;; in order
(defmethod materialize-documentation ((serial serial-block-t)
				      &key
					(format :latex)
					(stream nil)
					(depth nil))
  (dolist (b (slot-value serial 'blocks))
    (materialize-documentation b
			       :format format
			       :stream stream
			       :depth depth)))
						       

;;========================================================================
;; Materialize documentation for a section-t.
;; We internally materialize all blocks and concatenate the
;; documentation
(defmethod materialize-documentation ((section section-t)
				      &key
					(format :latex)
					(stream nil)
					(depth nil))
  (let ((s (if (null stream) (make-string-output-stream) stream)))
    ;; take care of the section title and label
    (let* ((section-depth-string (make-string (1+ (or depth 0))
					      :initial-element #\#))
	   (text (make-text (format nil "~A ~A {#~A}~%~%"
				    section-depth-string
				    (title section)
				    (label section))
			    :format :markdown)))
      (materialize-documentation text :format format :stream s :depth depth))
    ;; materialize the inner blocks
    (dolist (blk (blocks section))
      (when (block-needs-leading-newline blk)
	(terpri s))
      (materialize-documentation blk :format format :stream s :depth (1+ (or depth 0)))
      (when (block-needs-trailing-newline blk)
	(terpri s)))
    (if (null stream)
	(get-output-stream-string s)
	nil)))

(defmethod block-needs-leading-newline ( (section section-t) &key format)
  t)


;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================


