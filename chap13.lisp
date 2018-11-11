;; let's create a web server, pages 253 - 267

;; convert a two-digit hex value to its corresponding ASCII character, page 259
;;
;; the first and second digits are provided as the values of `c1` and `c2`,
;; respectively.  The function begins by using `coerce` to construct a
;; length-two string from the characters `c1` and `c2, which is then parsed
;; using `parse-integer` and bound to the local variable `code`.  Then the call
;; to `code-char` is used to convert this value to an ASCII character string.
(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
	       (coerce (list c1 c2) 'string)
	       :radix 16
	       :junk-allowed t)))
    (if code
	(code-char code)
	default)))


;; parse a string that possibly includes HTTP escape sequences, and convert it
;; back to regular text, pages 259 - 260
;;
;; the `labels` command begins by creating a local function `f` that accepts a
;; list of characters `lst` as input.  `f` then parses the first few characters
;; in the list (the amount depends on the input), and conses the parsed result
;; with result of a recursive call to `f` with the remainder of the list as its
;; input.
;;
;; in more detail, there are three cases to consider.  In the first case, the
;; first element of `lst` is a `%`, meaning that this is part of a hex escape
;; sequence.  In this case, the function `http-char` is invoked to perform the
;; conversion, with the two characters following the `%` in `lst` provided to it
;; as arguments.
;;
;; in the second case, we receive a `+` character as the first element of `lst`,
;; which according to the HTTP rules of encoding represents a space character.
;; In the third case there is no escape sequence so we can simply take the first
;; element of `lst` literally.
;;
;; then in the body of the `labels` call, we start by by using the inner call to
;; `coerce` to create a list of characters from the string `s`, which is then
;; passed to `f` for parsing, and then the outer call to `coerce` is invoked to
;; construct a string from the list.
(defun decode-param (s)
  (labels ((f (lst)
	     (when lst
	       (case (car lst)
		 (#\% (cons (http-char (cadr lst) (caddr lst))
			    (f (cdddr lst))))
		 (#\+ (cons #\space (f (cdr lst))))
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))


;; extract any `name=value` pairs and store as an alist with elements of the
;; form `(NAME . "value")`, page 261
;;
;; the call to `let*` begins by searching the input string `s` for the indices
;; of the first occurrence of `=` and of `&`, and binding the results to `i1 and
;; `i2` respectively
;;
;; next, the call to `cond` checks whether an `=` (i.e. a `name=value` pair) was
;; found.  If so, then the call to `(subseq s 0 i1)` obtains the "name" portion,
;; which is then first converted to uppercase letters (via `string-upcase`) and
;; then into a Lisp symbol (via `intern`).  Additionally, the "value" portion of
;; the pair is extracted with the call to `(subseq s (1+ i1) i2)`, and then HTML
;; escape sequences are converted using `decode-param`.  These two values are
;; then made into an alist element by using `cons`.
;;
;; as part of the same `cond` statement, the call to `and` is used to check and
;; see if `s` contains a `&` character (and hence there is more than one
;; `name=value` pair).  If that is the case, then `subseq` is used to obtain the
;; portion of the string after the `&`, and the result is provided as an
;; argument to a recursive call to `parse-params`, which is consed with the
;; previously obtained pair.
;;
;; if on the other hand, the original `cond` test for the presence of a `=` in
;; `s` was false, then we check whether `s` is the empty string, in which case
;; we return nil.  For the default case (i.e. a nonempty string without any
;; `name=value` pairs), the string is returned unchanged.
(defun parse-params (s)
  (let* ((i1 (position #\= s))
	 (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))


;; extract the pathname from an HTML request return as the first element of a
;; list along any `name=value` pairs as the remaining elements of the list, in
;; the form `(NAME . "value")`.  Page 261.
;;
;; in the first section of the `let*` statement, the first word and character
;; after it, and the last word are stripped from `s` and bound to `url`.  So for
;; example, `GET /docs/index.html?search=t HTTP/1.1` will be parsed as
;; `/docs/index.html?search=t`.  Note that this will throw an error if there are
;; less than two spaces in `s`.
;;
;; next, `x` is bound to the first occurrence of `?` in `url`, (or nil if there
;; is no occurrence).  Furthermore, if there is an occurrence, then we split
;; `url` into the portion before the `?`, and cons it with the portion after the
;; `?` and after being parsed by `parse-params`.  Otherwise, if there is no
;; occurrence, then simply cons `url` with nil, and in either case return the
;; resulting value.
(defun parse-url (s)
  (let* ((url (subseq s
		      (+ 2 (position #\space s))
		      (position #\space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
	(cons url '()))))


;; parse a stream where each line is of the form `key: value`, and convert into
;; a list where each element is of the form `(KEY . "value")`, page 262
;;
;; the call to `let*` begins by reading the first line of `stream` and binding
;; it to `s`.
;;
;; then, in the subcall to `let`, `i` is bound to the index of the first
;; occurrence of `:` in `s`, or nil if there is no such occurrence.
;; furthermore, if there is an occurrence, then the section of the string before
;; the `:` is extracted and converted into all uppercase letters, and then the
;; call to `intern` converts the string to a symbol.  This term is then consed
;; to the remainder of the string after the `:` and another character
;; (presumably a space in a well-formatted request), and the resulting list is
;; returned and bound to `h`.  On the other hand, if there was no `:` then the
;; inner call to `let` returns nil which is bound to `h`.
;;
;; then, when `h` is non-nil it is consed to the result of the recursive call to
;; `get-header`.  If `h` is nil, then we assume that there is no more data to
;; parse (due to the form of an HTTP request), and we have reached the
;; terminating state of the recursion.
(defun get-header (stream)
  (let* ((s (read-line stream))
	 (h (let ((i (position #\: s)))
	      (when i
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))


;;
;;
;; starts by searching the alist input `header` and binding the value associated
;; with `content-length` to `length`, or nil if there is no such entry.  If
;; `length` is nil, then the conditional for `when` fails and the entire
;; function returns nil.
;;
;; if, on the other hand, a value was obtained for `length`, then the call to
;; `make-string` creates a string of the length specified by the result of
;; `(parse-integer length)` Then, the call to `read-sequence` fills the
;; characters from `stream` into the string.  Finally, we invoke our
;; `parse-params` function with the string as our argument and return the result
;; from the main function.
(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(parse-params content)))))

(defvar teststring)

(setq teststring "GET /docs/index.html HTTP/1.1
Host: www.nowhere123.com
Accept: image/gif, image/jpeg, */*
Accept-Language: en-us
Accept-Encoding: gzip, deflate
User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)
(blank line)")


;; use quicklisp to load usocket
(ql:quickload "usocket")


(defun serve (request-handler)
  (let ((socket (usocket:socket-listen "127.0.0.1" 8080 :reuse-address t)))
    (unwind-protect
         (loop (with-open-stream (stream (usocket:socket-stream (usocket:socket-accept socket)))
                 (let* ((url (parse-url (read-line stream nil)))
                        (path (car url))
                        (header (get-header stream))
                        (params (append (cdr url) (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (usocket:socket-close socket))))


(defvar hello-html-form "<!DOCTYPE html>
<html>
  <body>
    <form>What is your name?
      <input name='name' />
    </form>
</html>
")


(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
	(if (not name)
	    (princ hello-html-form)
	    (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry... I don't know that page.")))

;; http://127.0.0.1:8080/greeting
