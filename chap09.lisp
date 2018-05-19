;; advanced datatypes and generic programming, pages 153 - 189

;; global variables for players and monsters, page 173
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)


;; game driver function.  Performs initialization and then calls the main game
;; loop, and prints a message depending on the outcome of the game.  Page 174.
(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))


;; the main game loop.  Recursively calls itself until the outer call to
;; `unless` fails.  Pages 174-175.
;;
;; The body of the `unless` call starts with a call to `show-player`.
;;
;; The call to `1+` performs the mathematical function `f(x) = 1 + floor(max(0,
;; x) / 15)`.  The result of this expression specifies the number of iterations
;; performed by `dotimes`, and `k` stores the index of the current iteration,
;; although it is not used .  The body of `dotimes` conditionally calls
;; `show-monsters` and `player-attack` based on the value returned by
;; `monsters-dead`.
;;
;; Next, `fresh-line` is called.
;;
;; After that, the call to `map` steps through *monsters* and for each element
;; calls `monster-dead`, and if that function returns nil, then it also calls
;; `monster-attack`.
(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    (fresh-line)
    (map 'list
	 (lambda(m)
	   (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    (game-loop)))


;; initialize player attributes, page 175
(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))


;; tests whether player has died, page 175
(defun player-dead ()
  (<= *player-health* 0))


;; display the player's attributes, page 176.
(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))


;; if `n` is is a positive integer, then returns a random integer in the range
;; of [1, n], otherwise returns `1`.  Page 177.
(defun randval (n)
  (1+ (random (max 1 n))))


;; Prompts the player to choose an attack, and performs the desired attack.
;; Page 176.
;;
;; The `princ` call displays text prompting the player to make a move.  Then
;; `read` is called and the result is passed to `case`.  The possible moves are
;; `s` for stab, `d` for double swing, and the `otherwise` case for a roundhouse
;; swing.
;;
;; The stab attack calls `monster-hit`, attacking a single monster chosen by
;; `pick-monster`.  The strength of the attack is obtained in the call to `+`,
;; and is given by 2 plus a random integer in the range of 1 and
;; floor(*player-strength* / 2), inclusive.  Recall that `ash` performs a
;; bit-shift in the amount given by its second argument.
;;
;; The double swing is a weaker attack than the stab attack, but has the
;; advantage of hitting two monsters instead of one.  An additional benefit of
;; the attack is that the player is told the strength of the attack before
;; selecting the monsters to attack, with the strengths being the same randomly
;; chosen integer taking a value between 1 and floor(*player-strength* / 6),
;; inclusive.  The first call to `monster-hit` initiates the first attack, and
;; if there are any monsters left, a second call to `monster-hit` is performed.
;;
;; The roundhouse swing performs a random number of attacks, with each attack
;; targeting a randomly chosen monster but only with strength 1.  The number of
;; attacks is a randomly chosen integer between 1 and floor(*player-strength* /
;; 3).

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "Your double swing has a strength of ")
	 (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monsters-dead)
		   (monster-hit (random-monster) 1))))))


;; select a random monster, page 177
;;
;; The call to `aref` selects a monster at the index returned by the call to
;; `random`.  If the monster chosen is not yet dead then this is the value
;; returned by the function, otherwise a new monster is chosen by recursively
;; calling `random-monster`.
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))


;; prompts the player for the monster that they wish to attack, and ensures that
;; they make a valid selection, page 178
;;
;; the `if` statement checks that the user input is parsed as a valid integer
;; between 1 and *monster-num*, and if so it also checks that the monster is not
;; already dead with a nested `if` call.  If both of these checks are passed,
;; then the selected monster is returned, otherwise `pick-monster` is
;; recursively called.
(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
	(progn (princ "That is not a valid monster number.")
	       (pick-monster))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (princ "That monster is alread dead.")
		     (pick-monster))
	      m)))))


;; initialize the *monsters* array, page 178
;;
;; the call `map` creates a `vector` (which is a 1-dimensional array) with
;; length *monster-num*, and fills each element by randomly choosing an item
;; from *monster-builders*
(defun init-monsters ()
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array *monster-num*))))


;; check if monster is dead, page 179
(defun monster-dead (m)
  (<= (monster-health m) 0))


;; check if every monster is dead, page 179
(defun monsters-dead ()
  (every #'monster-dead *monsters*))


;; display a listing of all of the monsters, page 179
;;
;; the call to `map` steps through *monsters*, and prints information about each
;; monster.  The value of `x` is used to print a number for each monster, and is
;; incremented in-place through the call to `incf`.  If the monster is dead,
;; then the word **dead** is printed, otherwise, the health and a description of
;; the monster are printed (using `monster-health` and `monster-show` to get the
;; necessary information).
(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "   ")
	   (princ (incf x))
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**dead**")
	       (progn (princ "(Health=")
		      (princ (monster-health m))
		      (princ ") ")
		      (monster-show m))))
	 *monsters*)))


;; create a `monster` data type with a health field that takes the value
;; obtained by calling `(randval 10)` at initialization time, page 180
(defstruct monster (health (randval 10)))


;; take `x` health points away from monster `m`, page 180
;;
;; the call to `decf` decrements (in place) the monster's health obtained
;; through the call to `monster-health` by `x` health points.  Then a message is
;; printed stating either that the monster was killed, or how many health points
;; were knocked off.
(defmethod monster-hit (m x)
    (decf (monster-health m) x)
    (if (monster-dead m)
	(progn (princ "You killed the ")
	       (princ (type-of m))
	       (princ "! "))
	(progn (princ "You hit the ")
	       (princ (type-of m))
	       (princ ", knocking off ")
	       (princ x)
	       (princ " health points! "))))


;; generic function printing the monster type, page 181
(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))


;; generic function placeholder for a monster attack on the player.  Will be
;; specialized later for various monsters.  Page 181.
(defmethod monster-attack (m))


;; create an `orc` data type that inherits all the fields of `monster` (due to
;; the `:include` option) and adds a field `club-level` that is initialized to
;; `(randval 8)`.  This call to `defstruct` automatically creates a function
;; `make-orc`, which we then push to *monster-builders*.
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)


;; create a specialized version of `monster-show` for orcs, page 182
(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))


;; create a specialized version of `monster-attack` for orcs.  The orc
;; decrements a random integer value between 1 and the orc's `club-level` field
;; from the player's health.  Note that the value of `club-level` is obtained
;; from the automatically generated `orc-club-level` function.  Page 182.
(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))


;; create a `hydra` data type that inherits from `monster` and add it to
;; *monster-builders*, page 183
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)


;; create a specialized version of `monster-show` for hydras, page 183
(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))


;; create a specialized version of `monster-hit` for hydras, page 183
;;
;; the `health` field of the hydra is decremented in-place by `x` units through
;; the call `decf`, and a message of the status of the hydra is printed
(defmethod monster-hit ((m hydra) x)
    (decf (monster-health m) x)
    (if (monster-dead m)
	(princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
	(progn (princ "You lop off ")
	       (princ x)
	       (princ " of the hydra's heads! "))))


;; create a specialized version of `monster-attack` for hydras, page 183
;;
;; The amount of damage caused by the attack is given by a random integer value
;; between 1 and floor(health / 2), inclusive, and where health refers to the
;; hydra's health.  The health of the player is decremented by this amount
;; through the call to `incf`.  Additionally, the health of the hydra (which is
;; equivalent to the number of heads) is incremented by 1 through the call to
;; `incf`.
(defmethod monster-attack ((m hydra))
    (let ((x (randval (ash (monster-health m) -1))))
      (princ "A hydra attacks you with ")
      (princ x)
      (princ " of its heads! It also grows back one more head! ")
      (incf (monster-health m))
      (decf *player-health* x)))


;; create a `slime-mold` data type that inherits all the fields of `monster`
;; (due to the `:include` option) and adds a field `sliminess` that is
;; initialized to `(randval 5)`.  This call to `defstruct` automatically creates
;; a function `make-slime-mold`, which we then push to *monster-builders*.
(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)


;; create a specialized version of `monster-show` for slime molds, page 183
(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))


;; create a specialized version of `monster-attack` for slime molds, page 183
;;
;; the slime molds decrement *player-agility* by a random integer value between
;; 1 and the value of the mold-sliminess field.  It also decrements with 50%
;; probability the player's health by 1 health point.
(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))


;; create a `brigand` data type that inherits from `monster` and add it to
;; *monster-builders*, page 185
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)


;; create a specialized version of `monster-attack` for brigands, page 183
;;
;; a brigand attack takes 2 points off of the player's attribute with the
;; highest value.  If several of the attributes are equally large, the brigand
;; will choose health over agility and agility over strength as the focus of
;; attack.
(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
	   (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
	   (decf *player-health* 2))
	  ((= x *player-agility*)
	   (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
	   (decf *player-agility* 2))
	  ((= x *player-strength*)
	   (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
	   (decf *player-strength* 2)))))
