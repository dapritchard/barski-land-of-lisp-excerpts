;; interacting with the world: reading and printing in Lisp, pages 85 - 106

(load "chap05.lisp")

;; page 94
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))


;; `game-read` prompts the user for input, then wraps the input in opening and
;; closing parentheses, and turns the resulting string into an `eval`uatable
;; list.  Pages 94-95.
;;
;; The call to `concatenate` places parenthesis around the input string and the
;; call to `read-from-string` converts the resulting string into a list.
;;
;; The function `quote-it` returns `(quote x)` which is equivalent to 'x.  I'm
;; not sure why it is defined as `(list 'quote x)` rather than just `(quote x)`.
;;
;; The `mapcar` call quotes every term in `cmd` starting with the second one,
;; and then the `cons` call prepends the first term to the list.
(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


;; variable specifying the commands that a user is allowed to input
(defparameter *allowed-commands* '(look walk pickup inventory))


;; check whether the command specified by `sexp` is one of the allowed commands
;; before invoking `eval`
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

;; `tweak-text` steps through `lst` a list of characters, and converts the
;; appropriate characters to uppercase or lowercase while leaving punctuation
;; unchanged.  Pages 97-98.
;;
;; The main idea being used is that stepping through the list is achieved by
;; picking off the first character and processing that character, and then
;; recursively calling the function on the remainder of the list and prepending
;; the character to the returned value.
;;
;; Two Boolean types of state are maintined through the parameters `caps` and
;; `lit`.  `caps` tracks whether it is the start of a sentance and that the
;; first letter should be capitalized, while `lit` tracks whether we are in the
;; middle of a pair of quotation marks, which signifies that we are to consider
;; the text literally.
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))           ; space
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))   ; end of sentence
	    ((eq item #\") (tweak-text rest caps (not lit)))                     ; start or end quote
	    (lit (cons item (tweak-text rest nil lit)))                          ; within pair of quotes
	    ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))  ; first letter of sentence
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))        ; other


;; `game-print` takes a list as input and prints it with appropriate
;; capitalizaiton.  Page 98.
;;
;; The call to `string-trim` removes any instances of the characters `(`, `)`,
;; or ` ` from the beginning or end of `(prin1-to-string lst)`.  The inner call
;; to coerce converts the resulting string to a list of characters, and then
;; `tweak-text` is called to process the characters, and then a second call to
;; `coerce` converts the list to a string.  Finally, the call to `princ` prints
;; the string, and `(fresh-line)` produces a newline.
(defun game-print (lst)
  (princ (coerce  (tweak-text (coerce (string-trim "() "
						   (prin1-to-string lst))
				      'list)
			      t
			      nil)
		  'string))
  (fresh-line))
