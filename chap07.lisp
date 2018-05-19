;; going beyond basic lists, pages 107 - 127

;; data that we are going to tranform to a graphviz graph, page 114
(defparameter *wizard-nodes* '((living-room (you are in the living-room.
					     a wizard is snoring loudly on the couch.))
			       (garden (you are in a beautiful garden.
					there is a well in front of you.))
			       (attic (you are in the attic. there
				       is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((living-room (garden west door) (attic upstairs ladder))
			       (garden (living-room east door))
			       (attic (living-room downstairs ladder))))


;; convert node identifiers into valid DOT identifiers, page 116
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))


;; parameter specifying length to truncate strings to, page 117
(defparameter *max-label-length* 30)
;; truncate expression to length given by *max-label-length*, page 117
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))


;; takes an alist and writes the name and contents in the form
;; `name[label="(name contents...)"];`, and where the contents are truncated to
;; a length no more than 30 chars.  These are used to create the node entries in
;; the DOT format.  Page 118.
(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))


;; steps through each element of `edges` and for each element prints one or more
;; lines with each line given by the first term in the element combined with the
;; remaining terms in the list.  If one of the remaining terms in the list is of
;; the form `(rem1 rem2...) then one line would be of the form
;; `first->rem1[label="rem2..."]`.  These are used to create the edge entries in
;; the DOT format.  Page 119.
(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	edges))


;; generate the directed graph DOT format, page 119
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


;; creates a file named `fname` and then executes `dot` with arguments `-Tpng`,
;; `-O`, and the value of `fname`.  The `with-open-file` call creates the file
;; and then the `sb-ext:run-program` call invokes `dot`.  Note that this code is
;; modified to work with SBCL.  Page 123.
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (sb-ext:run-program "/usr/bin/dot" (list "-Tpng" "-O" fname)))


;; creates a closure using `lambda` and passes it and `fname` to `dot-png`.  Has
;; the effect of creating a directed graph DOT and PNG files.  Page 123.
(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))


;; takes a nested alist and creates the edges of an undirected graph in DOT
;; format, page 125.
;;
;; The function `maplist` steps through a list as the data and supplies it to
;; the inner function after removing the first term from the previous list at
;; each step.  Then, before `mapc` gets the data, the call to `(cdar lst)` picks
;; off the first sublist in the list and strips off the item in the sublist
;; specifying the source location, returning a list with items specifying
;; destination locations.
;;
;; The `mapc` function then steps through those destination location sublists.
;; The call to `unless` checks to see whether the current destination location
;; is provided as a source location later in the list.  If so then that pair
;; will be specified later so nothing is done at that step (thus each pair is
;; printed the last time it is seen).  This seems to assume that each edge is
;; guaranteed to be specified in both directions in the data (right?).
;;
;; If the data gets past the `unless` condition, then it prints the source and
;; destination location as well as a node label.
(defun uedges->dot (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst)))
	   edges))


;; generate the undirected graph DOT format, page 125
(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))


;; creates a closure using `lambda` and passes it and `fname` to `dot-png`.  Has
;; the effect of creating an undirected graph DOT and PNG files.  Page 123.
(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))
