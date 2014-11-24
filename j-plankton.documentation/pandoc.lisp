
(in-package j-plankton.documentation)

;;========================================================================
(defparameter *format-to-pandoc-argument-mapping*
  (alexandria:plist-hash-table
   '(:markdown "markdown"
     :latex "latex"
     :html "html"
     :restructuredtext "rst"
     :rst "rst"))
  "A hashtable mapping keywords for format to actual commandline
   formats used by pandoc")

;;========================================================================
;; given a string, returns trhee values:
;;    the starting whitespace, or "" if none
;;    the trimmed string with no whitespace at start or end
;;    the ending whitespace, or "" if none
(defun trim-split (s)
  (let ((ret (list "" s "")))
    (cl-ppcre:register-groups-bind (start-ws trimmed end-ws)
	("^(\\s*)(.*?)(\\s*)$" s)
      (setf ret (list start-ws trimmed end-ws)))
    ret))
    
;;========================================================================
;; run the low-level pandoc executable with a given string, retuning
;; the resulting string.  The input format and output format are
;; specified
(defun %translate-using-pandoc (&key
				  input-utf-8
				  (input-format :markdown)
				  (output-format :latex))
  (when (eql input-format output-format)
    (return-from %translate-using-pandoc input-utf-8))
  (let ((input-format-pandoc (gethash input-format
				      *format-to-pandoc-argument-mapping*
				      nil))
	(output-format-pandoc (gethash output-format
				       *format-to-pandoc-argument-mapping*
				       nil)))
    (unless input-format-pandoc
      (error "Unknown input format: ~S" input-format))
    (unless output-format-pandoc
      (error "Unknown output format: ~S" output-format))
    (destructuring-bind (start-ws trimmed-input end-ws)
	(trim-split input-utf-8)
      (multiple-value-bind (pandoc-output pandoc-error return-code)
	  (with-input-from-string (in trimmed-input)
	    (uiop/run-program:run-program
	     (list "pandoc"
		   "-f"
		   input-format-pandoc
		   "-t"
		   output-format-pandoc)
	     :input in
	     :output :string))
	(concatenate 'string
		     start-ws
		     (string-right-trim '(#\Newline) pandoc-output)
		     end-ws)))))
  

;;========================================================================
;; returns if the given string is all whitespace
(defun is-all-whitespace (s)
  (cl-ppcre:scan "^\\s+$" s))

;;========================================================================
;; given a list, split it into even elements and odd elements,
;; returns (values even-els odd-els)
(defun split-even-odd (x)
  (let ((even nil)
	(odd nil))
    (loop for i upto (length x)
	  for e in x
	  do
	  (if (evenp i) 
	      (push e even)
	      (push e odd)))
    (values 
     (nreverse even)
     (nreverse odd))))

;;========================================================================
;; Given a string, looks for the smallest set of newlines followed by
;; whitespace and removes these whitespaces.  This is so that we
;; have strings with newlines and whitespaces to make it look nice
;; in the editor but keeps the original structure.
(defun %strip-whitespace-smartly (text)
  (when (null text)
    (return-from %strip-whitespace-smartly nil))
  (let* ((lines-and-spaces
	  (cl-ppcre:split "\\n([ \\t]*)" text :with-registers-p t)))
    (when (= 1 (length lines-and-spaces))
      (return-from %strip-whitespace-smartly text))
    (multiple-value-bind (lines spaces) (split-even-odd lines-and-spaces)
      ;; strip leading spaces of first line
      (when lines 
	(setf (car lines) (string-left-trim '(#\Space #\Tab) (car lines))))
      ;; find the minimum size whitespace region that contains
      ;; at least one whitespace
      (let* ((min-whitespace
	       (loop for ws in spaces
		     for line in lines
		     for i upto (length spaces)
		     when (and (not (null ws))
			       (or (equal line "")
				   (> (length ws) 0)))
		       minimize (length ws)))
	     ;; remove the min whitespace from spaces
	     (trimmed-spaces
	       (mapcar #'(lambda (ws)
			   (if (or (null ws)
				   (= (length ws) 0))
			       ws
			       (subseq ws min-whitespace)))
		       spaces)))
	
	(let ((res
	       (apply #'concatenate  'string
		      (alexandria:mappend
		       #'(lambda (line next-ws)
			   (list line (string #\Newline) next-ws))
		       lines
		       trimmed-spaces))))
	  (when (and lines
		     (> (length lines) (length trimmed-spaces)))
	    (setf res (concatenate 'string res (car (last lines)))))
	  res)))))
      
    

;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
;;========================================================================
