;; ramping lisp up a notch with functional programming, pages 291 - 303

					; the clean, functional part

;; return a list with the first element pointing to `widget`, and the remaining
;; elements are the elements in `database`.  Page 296.
(defun add-widget (database widget)
  (cons widget database))

					; the dirty, nonfunctional part (this
					; line and the definition of `main-loop`

;; define *database*, initialized as an empty list.  Page 296.
(defparameter *database* nil)

;; launches an infinite loop that prompts the user for the name of a new widget,
;; prepends the value provided by the user to the start of *database*, and then
;; prints out the value of the newly created list.  Page 296.
;;
;; recall that `~%` prints a newline, while `~a` prints the value of the
;; following argument in the list (i.e. *database*, in this case)
(defun main-loop ()
  (loop (princ "Please enter the name of a new widget:")
     (setf *database* (add-widget *database* (read)))
     (format t "The database contains the following:~%  ~a~%" *database*)))

;; initialize a list for some of the following examples
(defparameter *my-list* '(4 7 2 3))

					; For demonstration purposes only. A
					; Lisper would not write code like this.

;; each iteration adds 2 to *my-list* in place
(loop for n below (length *my-list*)
   do (setf (nth n *my-list*) (+ (nth n *my-list*) 2)))

;; `mapcar` steps through the input list `(4 7 2 3)`, and creates a new list
;; where each element has the value of 2 plus the corresponding element in the
;; original list
(mapcar (lambda (x)
	  (+ x 2))
	'(4 7 2 3))
