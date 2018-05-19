;; looping with the loop command, pages 195 - 235

;; global simulation parameters, page 203
;;
;; *width* and *height* define the size of the world.  *jungle* defines the
;; coordinates of a rectangle within the map that the junge resides in.  The
;; first two values specify the (x, y) coordinates of the top-left position in
;; the map, while the remaining two values specify the width and height of the
;; jungle, respectively.  *plant-energy* specifies the amount of days that
;; eating a plant will sustain an animal
(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)


;; create a hash table used to store the plants in our world.  We're going to
;; use the (x, y) coordinates as the keys, so we use `equal` as our test.  Page
;; 204.
(defparameter *plants* (make-hash-table :test #'equal))


;; add a plant at random within the rectange specified by the input parameters,
;; where the grid specification is the same as that described in the definition
;; of *jungle*.  Page 204
;;
;; the position of the plant is found be starting at the top left corner of the
;; allowed rectangle and then adding a random amount of width and height.  The
;; the result is stored to the variable `pos`, and then added to *plants*.
;;
;; note that this is a noop if there is already a plant at that position.
(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))


;; add a plant at random both in the jungle and in the entire map, page 204
;;
;; `apply` is used as a device to pass the parameters in from jungle to
;; `random-plant`.  Note that either call to `random-plant` may be a noop if
;; there happens to already be a plant at the randomly chosen location.
(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))


;; create an animal object, page 205
;;
;; the `x` and `y` coordinates track the current location of the animal.  The
;; `energy` field tracks how many more days the animal can live without food
;; without dying.  The `dir` field specifies the direction that the animal will
;; move in the next turn.  The field can take integer values 0 through 7 meaning
;; that the animal move in the following direction on the map (where the center
;; is the current location):
;;
;;     0   1   2
;;     7       3
;;     6   5   4
;;
;; The `genes` field will be represented using a length-8 list, which will in
;; effect provide the probability of the next value of `dir`.  The way this will
;; work is that each element in `genes` will have a positive integer value, and
;; the probability of turning to direction k is given by the normalized value of
;; the k-th element of the list.  So for example if the list was (2 4 6 8 10 12
;; 14 16) then the probability of turning to position 2 (i.e. the top-right)
;; would be 6 / (2 + 4 + 6 + 8 + 10 + 12 + 14 + 16).
(defstruct animal x y energy dir genes)


;; create an initial animal to populate the planet with, page 207
;;
;; the parameter specifications place the animal in the middle of the world
;; (which is also the middle of the jungle), with 1000 energy and a direction
;; pointing to the top-left.  The elements of the list specified for `genes` are
;; each given a random integer value between 1 and 10, inclusive.
(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
		     :y (ash *height* -1)
		     :energy 1000
		     :dir 0
		     :genes (loop repeat 8
			       collecting (1+ (random 10))))))


;; move the animal based on the direction it is facing, and decrements its
;; health, pages 207 - 208
;;
;; the first call to `setf` changes the x-coordinate for the animal.  The inner
;; call to `cond` obtains the value that we add to the current x-coordinate,
;; which is either -1, 0, or 1, depending on whether we move left, up or down,
;; or right.  Also note that *width* is added to the sum of these two terms, and
;; then the total is taken modulo *width*.  The modulo is because the world
;; wraps around, and the reason that we add the *width* term is just a
;; mathematical trick to obtain the proper number when we wrap to the left.
;;
;; A similar procedure is performed to modify the y-coordinate as part of the
;; second `setf` call, and then `decf` is invoked to deplete the animal's energy
;; by 1 unit.
(defun move (animal)
  (let ((dir (animal-dir animal))
	(x (animal-x animal))
	(y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
				    (cond ((and (>= dir 2) (< dir 5)) 1)
					  ((or (= dir 1) (= dir 5)) 0)
					  (t -1))
				    *width*)
				 *width*))
    (setf (animal-y animal) (mod (+ y
				    (cond ((and (>= dir 0) (< dir 3)) -1)
					  ((and (>= dir 4) (< dir 7)) 1)
					  (t 0))
				    *height*)
				 *height*))
    (decf (animal-energy animal))))


;; randomly rotates the `dir` field for the animal from its current angle based
;; upon the probabilities specified by the `genes` field, pages 208 - 209
;;
;; the call to `random` obtains an integer value between 0 and the sum of the
;; animal's genes, and which is then bound to `x`.  This number determines how
;; much the animal will turn based on `argmin_k sum_{i=0}^k genes_k > x`, and
;; where genes_k denotes the k-th element of the animal's `genes` field.
;;
;; the `labels` call creates a local function `angle`, which calculates the
;; amount the animal turns according to the formula described above.  The
;; function works by taking an initial argument for the animal genes named
;; `genes`, as well as an argument `x`, and then recursively calling itself
;; until the correct turn number is found.  For each call to `angle` it
;; subtracts the value of the first element of `genes` from `x`, and stores the
;; result as `xnu`.  If `xnu` is negative, then the function returns 0.
;; Otherwise, the function adds 1 to the result of the recursive call to `angle`
;; with `(cdr genes)` and `xnu` as its arguments.  Thus in effect it subtracts
;; the i-th element of `genes` from the running total of `x`, and keeps track of
;; the number of times that this calculation is performed.
;;
;; The call to `setf` updates the `dir` field for the animal with the value of
;; the current `dir` field plus the result from the call to `angle`, modulo the
;; total number of directions (i.e. 8) in order to implement wrapping around the
;; circle.
(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
	       (let ((xnu (- x (car genes))))
		 (if (< xnu 0)
		     0
		     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
	    (mod (+ (animal-dir animal)
		    (angle (animal-genes animal) x))
		 8)))))


;; checks to see if a plant exists at the animal's current location, and if so
;; then increments the animal's energy and removes the animal from the map, page
;; 209.
;;
;; the call to `let` creates the local variable `pos` which is a list of the x
;; and y coordinates of the animal.  Then the call to `gethash` checks to see if
;; there is a plant at that location, and if so then the animals `energy` field
;; is incremented by 1 and the plant is removed from *plants*.
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))


;; specifies a minimum amount of energy required for a parent to reproduce, page
;; 210
(defparameter *reproduction-energy* 200)


;; conditionally spawn a child animal if the current animal has enough energy,
;; page 210
;;
;; first the animal's `energy` field is stored as the local variable `e` as part
;; of the call to `let`.  Then if the animal has at least *reproductive-energy*
;; units, we begin the process of reproduction.
;;
;; the reproductive process costs the animal half of its energy, which is
;; implemented with the first call to `setf`.  Then a copy of the parent is made
;; using the `copy-structure` command, and the result is stored as the local
;; variable `animal-nu`.  Additionally, a copy of the animals `genes` field is
;; created using the call to `copy-list`, and the result is stored as the local
;; variable `genes`.  As part of the same call to `let`, a random integer value
;; between 0 and 7 is bound to the local variable `mutation`.
;;
;; note that the `copy-structure` command only makes a shallow copy.  This is
;; why a copy of the animal's genes was made: so that we can replace the pointer
;; in the copied data to the parent's genes with a pointer to the new copy of
;; the genes.
;;
;; next, mutation is introduced into the copied genes.  The call to `nth` uses
;; the value of `mutation` to specify which of the genes is to be mutated as
;; part of a call to `setf`.  `nth` is used again to obtain the value of the
;; `mutation`-th genes which then has a random number with values in { -1, 0, 1
;; } added to it, and then `max` is used to ensure that the value is at least 1.
;;
;; The last call to `setf` then links the child animal's `genes` field to the
;; mutated genes, and finally whole structure is added to the *animals* list.
(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
	    (genes (copy-list (animal-genes animal)))
	    (mutation (random 8)))
	(setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
	(setf (animal-genes animal-nu) genes)
	(push animal-nu *animals*)))))


;; simulate a day in the world, page 212
;;
;; first any dead animals are removed from list of *animals* by using the call
;; to `remove-if` and assigning the result via `setf`.  Next, `mapc` is used to
;; step across the list of animals, and for each animal, turn, move,
;; conditionally feed, and conditionally reproduce it.  Finally, plants are
;; added to the map
(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
			       (<= (animal-energy animal) 0))
			     *animals*))
  (mapc (lambda (animal)
	  (turn animal)
	  (move animal)
	  (eat animal)
	  (reproduce animal))
	*animals*)
  (add-plants))


;; draw a map of the world, pages 212 - 213
;;
;; the function draws the map one character at-a-time using an outer loop
;; indexing the rows and an inner loop indexing the columns.  The outer loop
;; begins by starting a fresh line and prints a `|` to represent the left border
;; of the map.
;;
;; then control enters the inner loop and begins stepping through the columns of
;; the map.  At each (x, y) coordinate, the call to `some` steps through
;; *animals*, and checks to see if there is an animal at the current location.
;; If there is an animal then the `M` charcter is printed at that location.  If
;; on the other hand there isn't an animal, then the call to `gethash` checks to
;; see if there is a plant at that location, and if so then prints a `*`.
;; Otherwise a space is printed.
;;
;; when the inner loop completes, then a `|` is printed to represent the right
;; border of the map.
(defun draw-world ()
  (loop for y
     below *height*
     do (progn (fresh-line)
	       (princ "|")
	       (loop for x
		  below *width*
		  do (princ (cond ((some (lambda (animal)
					   (and (= (animal-x animal) x)
						(= (animal-y animal) y)))
					 *animals*)
				   #\M)
				  ((gethash (cons x y) *plants*) #\*)
				  (t #\space))))
	       (princ "|"))))


;; create an interface and driver for the animal simulation, page 213
;;
;; first the function draws the current state of the world via the call to
;; `draw-world` and then starts a new line.
;;
;; next, the function inokes `read-line` to obtain input from standin.  If the
;; input is `quit`, then the function ends.
;;
;; if on the other hand the input was not `quit`, then body of the second case
;; for `cond` parses the input as an integer, allowing for trailing
;; non-numerical characters by using the `junk-allowed` option.  Next, if the
;; parsed value bound to `x` is positive, then the call to `loop` loops `x`
;; times, indexing the iteration with `i`.  Each iteration calls `update-world`
;; to advance the world by one day, and furthermore if `i` is evenly divisible
;; by 1000 then a `.` is printed to give some feedback to the user as to the
;; progress of the evolution.
;;
;; finally, the function recursively calls itself in order to print the updated
;; map and allow for more user input
(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (let ((x (parse-integer str :junk-allowed t)))
	       (if x
		   (loop for i
		      below x
		      do (update-world)
		      if (zerop (mod i 1000))
		      do (princ #\.))
		   (update-world))
	       (evolution))))))
