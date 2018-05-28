;; a game where the player has to strategically move around the grid while being
;; chased by murderous robots.  If the player runs into a robot then he or she
;; loses the game, and the player wins the game by destroying all of the robots.
;; The robots can be killed by tricking two into running into each other, which
;; kills both and creates a scrap heap.  Furthermore, if yet another robot runs
;; into a scrap heap, then that robot will also die.
;;
;; the player can move to any of the eight surrounding squares on the map, or
;; they can teleport to a random location in the map.
(defun robots ()
  ;; the `named` syntax allows us to name the loop (in this case `main`), and
  ;; later invoke `return-from` with `main` as an argument to exit from the loop
  (loop named main
     ;; a local variable with each element of the form (key . direction), and
     ;; representing the eight surrounding directions that a player or robot can
     ;; move on the grid.  The value corresponding to `key` represents the key
     ;; that the player need to press to move in that direction.
     ;;
     ;; to understand the value corresponding to `direction`, first we need to
     ;; know that the map is represented internally as a one-dimensional arlray
     ;; using row-major ordering.  Then the value corresponding to `direction`
     ;; represents the number of elements in the array player or robot need to
     ;; move.  So for example to move left, the number of elements would be -1,
     ;; but to move down it would be 64 (since the width of the grid is 64).
     ;;
     ;; thus the directions are in order, top-left, top, top-right, left, right,
     ;; bottom-left, bottom, bottom-right
     with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
			 (d .   1) (z .  63) (x .  64) (c . 65))
     ;; the first of two `for` constructs, this one using equals-then form.  The
     ;; variable `pos` is initialized at position 544, which is 544 / 64 = 8.5
     ;; lines down the board.  The `then` part of the statement is executed once
     ;; for each iteration of the loop.
     ;;
     ;; inside the `then` clause, the prompt is printed with `format`, the
     ;; buffer is flushed with `force-output`, and the user input is read and
     ;; bound to `c`.  Then the value of `d` is bound to value the call to
     ;; `assoc`.
     ;;
     ;; next, if `d` is non-empty then the input was a movement command and we
     ;; return the value of pos after incrementing it by the appropriate amount.
     ;; Otherwise, if the value of `c` was `t` then we return a random value for
     ;; `pos`.  Otherwise, if the value of `c` was `l`, then we return from the
     ;; loop.  Otherwise, we simply return the value of `pos` (i.e. a noop).
     for pos = 544
     then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
		 (force-output)
		 (let* ((c (read))
			(d (assoc c directions)))
		   (cond (d (+ pos (cdr d)))
			 ((eq 't c) (random 1024))
			 ((eq 'l c) (return-from main 'bye))
			 (t pos))))
     ;; the second of two `for` constructs, this one also using equals-then
     ;; form.  The variable `monsters` is initialized with 10 random board
     ;; positions.
     for monsters = (loop repeat 10
		       collect (random 1024))
     ;; next, an inner loop is performed inside the `then` statement.  The loop
     ;; steps through all of the robot positions.  If at a given position, there
     ;; is more than one robot, then at some point these robots have collided
     ;; (i.e. this position is a scrap heap).  The strategy taken in this case
     ;; is to simply not move the current robot.
     ;;
     ;; if, on the other hand the robot has not yet suffered a collision, then
     ;; we move the robot to try to get it closer to the player.  This is done
     ;; by moving the robot to one of the eight surrounding squares, choosing
     ;; one that has achieves the minimum Manhattan distance to the player's
     ;; location.
     ;;
     ;; in more detail, we start by invoking `loop` with two `for` statements.
     ;; The first `for` statement steps through each of the directions in
     ;; `directions`, where the `(k . d) in directions` syntax creates local
     ;; variables `k` and `d` taking the corresponding values of the current
     ;; `(key . direction)` element.
     ;;
     ;; then the second `for` statement creates a local variable `new-mpos` by
     ;; adding the value of `d` to the current position, and consing its value
     ;; with the its Manhattan distance to the player's position.  The Manhattan
     ;; distance is obtained by the adding distance of the columns (the first
     ;; `abs` call) and the distance of the rows (the second `abs` call).  The
     ;; column number of a position is obtained by using the `mod` command,
     ;; while the row number of a position is obtained by using the `ash`
     ;; command (i.e. the integer portion of the result of multiplying by 2^{-6}
     ;; = 1 /64).
     ;;
     ;; the resulting list from the loop is sorted by the distance and the new
     ;; position is extracted via the call to `cdar`
     then (loop for mpos in monsters
	     collect (if (> (count mpos monsters) 1)
			 mpos
			 (cdar (sort (loop for (k . d) in directions
					for new-mpos = (+ mpos d)
					collect (cons (+ (abs (- (mod new-mpos 64)
								 (mod pos 64)))
							 (abs (- (ash new-mpos -6)
								 (ash pos -6))))
						      new-mpos))
				     '<
				     :key #'car))))
     ;; conditionally return `player-wins` if the `when` statement evaluates to
     ;; true, i.e. when all of the robots have been killed.  This occurs when
     ;; every robot has had a collision, which is checked by seeing if the
     ;; position of every robot has at least one other robot at that position
     ;; (this check suffices because once two or more robots collide, they stop
     ;; moving)
     when (loop for mpos in monsters
	     always (> (count mpos monsters) 1))
     return 'player-wins
     ;; formatting string:
     ;;     ~%        opening newline
     ;;     |         the very first `|` printed
     ;;     ~{        start formatting loop
     ;;     ~<        start justification section
     ;;     |~%|      closing `|`, newline and opening `|`, for each line
     ;;     ~,65:;    ~:; specifies equally spaced pieces, and 65 is line width
     ;;     ~A        print without escape characters
     ;;     ~>        end justification section
     ;;     ~}        end formatting loop
     ;;     |         the very last | printed
     ;;
     ;; the `loop` command creates the data used by `format`.  It steps through
     ;; each position in the board, and either returns a `@`, `A`, `#`, or ` `,
     ;; representing the player, a robot, a scrap heap, or an empty space,
     ;; respectively.
     ;;
     ;; in more detail, the outer `cond` statement first checks to see if there
     ;; are any monsters at the current position.  If so, then it also checks to
     ;; see if the player is at the same position, and if so, invokes
     ;; `return-from` end the game.  If there's no player there, then the second
     ;; statement in the inner conditional checks to see if there's any other
     ;; monsters (i.e. it is a scrap heap), and if so returns `#`.  Otherwise,
     ;; it returns `A` to signify a single monster.
     ;;
     ;; if there's no monster at the positon, then we check to see if there's a
     ;; player at that position, and if so return `@`.  Otherwise we've covered
     ;; all of the other possibilities, to simply return a blank space ` `.
     do (format t
		"~%|~{~<|~%|~,65:;~A~>~}|"
		(loop for p
		   below 1024
		   collect (cond ((member p monsters)
				  (cond ((= p pos) (return-from main 'player-loses))
					((> (count p monsters) 1) #\#)
					(t #\A)))
				 ((= p pos) #\@)
				 (t #\ ))))))
