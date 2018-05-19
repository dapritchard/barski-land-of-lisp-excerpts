;; this ain't your daddy's wumpus, pages 129 - 152

;; load utilites for creating graphs
(load "chap07.lisp")

;; defining game parameters
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)
(defparameter *player-pos* nil)


;; return a random integer between 1 and *node-num*, inclusive.  Page 135.
(defun random-node ()
  (1+ (random *node-num*)))


;; if `a` and `b` are not the same then return a list with two pairs `(a . b)`
;; and `(b . a)`, otherwise return nil.  Page 135.
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))


;; creates a list with up to 2 times *edge-num* items and where every item is a
;; dotted pair of numbers from the set { 1, ..., *node-num* }.  Furthermore
;; every second item is the reverse ordering of the previous item, so for
;; example you might have the items (5 . 22) followed by (22 . 5), and dotted
;; pairs with the same value are not allowed.  Page 135.
;;
;; The loop expression calls `edge-pair` *edge-num* times and collects the
;; length-2 list of pairs (or nil) returned by the function into a list.  Then
;; the outer call to `apply` unpacks the length-2 pairs and appends them
;; together, which has the effect of tranforming a 3-level list into a 2-level
;; list.
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
		     collect (edge-pair (random-node) (random-node)))))


;; find all edges in `edge-list` that start from node `node`, page 138
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))


;; find all of the nodes that are reacheable from `node` by traversing the edges
;; in `edge-list`.  Page 138.
;;
;; The `let` call creates an empty list `visited` that is available to the body
;; of the function.  The inner call to `labels` creates the function `traverse`
;; that is then called with `node` as its argument.
;;
;; The function `traverse` starts by checking to see if the argument to `node`
;; has already been added to `visited`.  If not, then it adds `node` to
;; `visited` and calls `mapc` to step through each of the node's edges (obtained
;; through the call to `(direct-edges node edge-list)`), and recursively calls
;; `traverse` with the node that the current edge leads to.
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))


;; create a list such that each element is a sublist specifying the set of all
;; nodes that can be reached from any one of the nodes in the sublist.  In other
;; words, each element is an island of nodes that are connected within the
;; island, but are not connected to any of the nodes in other islands.  Page 138
;;
;; The call to `let` initiates an empty list `islands` that we are going to
;; store our islands in.  Then a local function `find-island` is defined and
;; invoked as part of the call to `labels`.
;;
;; Within the definition of `find-island`, the call to `get-connected` gets all
;; of the nodes that are connected to `(car nodes)`.  These connected nodes (i.e
;; an island) are prepended to `islands`, and then any remaining nodes that are
;; not part of the island are stored in `unconnected`.  If `unconnected` is
;; nonempty, then it is provided as the input to a recursive call to
;; `find-island`, otherwise the function ends.
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))


;; Create a list of edges that connects each of the islands in `island`, page
;; 138.
;;
;; The call to `edge-pair` returns a pair of edges connecting first node in the
;; first island to first node in the second island.  These two new edges are
;; then appended to the list returned by the recursive call to
;; `(connect-with-bridges (cdr islands))`.
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))


;; Appends enough edges to `edge-list` to ensure that all of the nodes in
;; `edge-list` are connected (this is effectively a noop if all of the nodes are
;; already connected).  Page 138.
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))


;; Creates an alist where the lookup key is the node, and the value is a list
;; all of the nodes that are directly connected to the node.  Page 139.
;;
;; The call to `remove-duplicates` returns a list of all of the nodes that are
;; in the map.  Then `mapcar` steps through this list one-at-a-time, providing
;; the current node as the argument to `node1` in the lambda function.
;;
;; The definition for the lambda function includes a call to `mapcar` that steps
;; through the list of nodes that share an edge with `node1` (this list is
;; obtained through the calls to `direct-edges` and `remove-duplicates`).  Then
;; the current node number is extracted by the call to `(cdr-edge)` and is
;; placed in a list, and then the resulting list of these lists returned by
;; `mapcar` is appended to `node1` by using the `cons` function and returned.
;;
;; Note that the reason that we place each of the destination nodes in its own
;; sublist is that later we will add a marker to some of these sublists to
;; denote the presence of cops.
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))


;; Returns the `edge-alist` after possibly after adding the cops flag to some of
;; the destination nodes.  Page 139.
;;
;; The outer call to `mapcar` steps through `edge-alist` one element at-a-time
;; and returns the element possibly after adding the cops flag to some of the
;; destination nodes if they are in the list given by `edges-with-cops`.
;;
;; The definition of the outer lambda function calls `let` and creates two local
;; variables `node1` (the source node) and `node1-edges` (the list of
;; destination nodes).  Then the inner call to `mapcar` steps through these
;; destination nodes.
;;
;; The inner call to `let` creates a local variable `node2` that which has the
;; value of the current destination node.  The call to `edge-pair` creates a the
;; two directional edges for `node1` and `node2`, and then the call to
;; `intersection` is used to see whether either of those pairs is in
;; `edges-with-cops`.  If so, then we return `(list node2 'cops)`, and otherwise
;; we return the unchanged value of `edge`.
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-alist))


;; Randomly generate a connected graph stored in the form of an alist with the
;; source node as the keys and nodes that can be traveled to from the node as
;; the values.  Page 139.
;;
;; The first item in `let*` creates a list of nodes and passes it to the call to
;; `connect-all-islands` along with the randomly generated edge list created by
;; `make-edge-list`.  The net result of the call to `connect-all-islands` is to
;; randomly generate an edge list that is guaranteed to be fully connected.
;;
;; The call to `remove-if-not` randomly keeps elements from `edge-list` with
;; probability `1 / *cop-odds*` by calling `(zerop (random *cop-odds*)`.  Then
;; the call to `edges-to-alist` creates the alist form of the map, which is then
;; passed as an argument to `add-cops` along with `cops` to inset to cops
;; information into the alist.
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		   collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))


;; obtain a list of the nodes that share an edge with `node`
(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))


;; returns nil if `a` and `b` do not share an edge, or a list with the first
;; element given by `b` if they do share an edge.  Thus the function can be used
;; as a predicate function.  Page 142.
(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))


;; returns nil if `a` and `b` are not connected in 2 or less moves, or a list
;; with the first element given by `b` if they do share an edge.  Thus the
;; function can be used as a predicate function.  Page 143.
;;
;; The `or` function first checks whether `a` and `b` share an edge by calling
;; `within-one`.  If they do not share an edge, then the function checks whether
;; they are exactly 2 moves apart.  First the list of nodes 1 move away is
;; obtained through the call to `neighbors`, and then the call to `some` steps
;; through this list and checks whether any of those nodes are 1 node away by
;; calling `within-one` at each step.
(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
	      (within-one x b edge-alist))
	    (neighbors a edge-alist))))


;; Create a list of sublists such that the first item in each sublist is the
;; node number, and the remaining items, if any, give you clues about what is
;; around you such as `wumpus`, `blood!`, `glow-worm`, `lights!`, or `sirens!`.
;; Page 143.
;;
;; The call to `random-node` randomly chooses a node as the location of the
;; Wumpus, and then the loop creates a list with nodes for the Glowworms.
;;
;; Within each loop iteration the `append` function creates a list with first
;; item the node number and followed by 0 or more terms.  The first `cond` call
;; creates a list `(wumpus)`, `(blood)`, or `()` depending on whether the Wumpus
;; is on the current node, is within 2 nodes of the current node, or is more
;; than 2 nodes away.
;;
;; The second `cond` call creates a list `(glow-worm)`, `(lights!)`, or `()`,
;; depending on whether there is a Glowworm at the current node, is within 1 of
;; the current node, or is more than 1 node away.
;;
;; The call to `when` checks to see if there are cops at any of the edges
;; connected to the current node and returns `(sirens!)` if so or `()`
;; otherwise.  It does this by getting the list of edges that leave the node
;; with the call to `(cdr (assoc n edge-alist))` and then steps through the
;; sublists and seeing if any of those have more that 1 item (recall that the
;; edges are either of the form e.g. `(5)` or `(5 cops)`).
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num*
		       collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
		       (cond ((eql n wumpus) '(wumpus))
			     ((within-two n wumpus edge-alist) '(blood!)))
		       (cond ((member n glow-worms)
			      '(glow-worm))
			     ((some (lambda (worm)
				      (within-one n worm edge-alist))
				    glow-worms)
			      '(lights!)))
		       (when (some #'cdr (cdr (assoc n edge-alist)))
			 '(sirens!))))))


;; Find a starting node that doesn't have any bad guys on it.  To test whether
;; it has bad guys it is enough to see if the node has more than one item in the
;; list with the call `(cdr (assoc x *congestion-city-nodes*))`.  Page 144.
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
	(find-empty-node)
	x)))


;; Use Graphviz to draw an undirected graph of the city.  Creates the files
;; `city` and `city.png`, which are the Graphviz file and resulting PNG map of
;; the city.  Page 145.
(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))


;; Randomly generate a game map, storing the information in the relevant
;; variables.  Additionally creates the files `city` and `city.png` through the
;; call to `draw-city`.  Note that we change the name slightly to avoid a name
;; clash with a later version of this function.  Page 145.
(defun new-game-v1 ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))


;; Get the list of visited nodes and nodes that are one move away from a visited
;; node.  The location of bad guys or clues are provided for a visited node, and
;; are hidden for non-visited nodes.  Page 146.
;;
;; The input data for the outer `mapcar` call is obtained by appending
;; *visited-nodes* with the list of all nodes that are 1 move away from any
;; visited nodes, and then removing duplicates.  The list of nodes 1 move away
;; is obtained through the call to `mapcan` which in turn gets a list of nodes 1
;; move away from the node `node` through the call to `mapcar`.
;;
;; The function argument to the outer `mapcar` call takes an argument `node` and
;; checks to see if the node has already been visited, and if not then it
;; returns `(list node '?)`.  If it has been visited, then it prints the node
;; number and the presence of any bad guys or clues.  Furthermore, if the player
;; is currently located at `node`, then an asterisk is appended to this list.
(defun known-city-nodes ()
  (mapcar (lambda (node)
	    (if (member node *visited-nodes*)
		(let ((n (assoc node *congestion-city-nodes*)))
		  (if (eql node *player-pos*)
		      (append n '(*))
		      n))
		(list node '?)))
	  (remove-duplicates
	   (append *visited-nodes*
		   (mapcan (lambda (node)
			     (mapcar #'car
				     (cdr (assoc node
						 *congestion-city-edges*))))
			   *visited-nodes*)))))


;; The outer call to `mapcar` steps through the list of visited nodes and conses
;; `node` with a list of nodes that share an edge with `node`, after stripping
;; the cops data from destination nodes that haven't yet been visited.  Page
;; 146.
;;
;; The inner call to `mapcar` takes the list of nodes that share an edge with
;; `node` as its input (obtained through the calls to `assoc` and `cdr`).  Then
;; the inner lambda function checks to see if the destination node has been
;; visited, and if so returns the value of the node and the presence of cops if
;; they are on the edge between the nodes.  Otherwise if the destination node
;; has not been visited then only the node number is returned (obtained through
;; the call to `(list (car x))`).
(defun known-city-edges ()
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*)
				     x
				     (list (car x))))
			       (cdr (assoc node *congestion-city-edges*)))))
	  *visited-nodes*))


;; Use Graphviz to draw an undirected graph of the known parts of the city.
;; Creates the files `known-city` and `known-city.png`, which are the Graphviz
;; file and resulting PNG map of the city.  Page 145.
(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))


;; Randomly generate a game map, storing the information in the relevant
;; variables.  Additionally creates the files `city` and `city.png` through the
;; call to `draw-city`, as well as the files `known-city` and `known-city.png`
;; through the call to `draw-known-city`.  Page 147.
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))


;; Tells the player the consequence of their move according to the game logic,
;; and updates the internal data structures accordingly.  The files `known-city`
;; and `known-city.png` are also generated to reflect the move.  The formal
;; arguments take as inputs the edge that the player is traveling for the new
;; move, the position of the player prior to the move, and a Boolean variable
;; specifying whether the player is charging the Wumpus.  Page 149.
;;
;; The first argument to `let*` creates the local variable `node` and which has
;; the value of a list with first element the node number and any remaining
;; elements being game information associated with the node.  Then the local
;; variable `has-worm` is created which is either (i) nil if the node does not
;; have a Glowworm or if we've already visited the list (Glowworms only attack
;; us once), or (ii) is a list with first element given by `glow-worm`.  Thus
;; `has-worm` may be used as a Boolean variable.
;;
;; The call to `pushnew` prepends `pos` to *visited-nodes* in place if `pos` is
;; not yet an element in the list, or alternatively it is just a noop.  Then the
;; call to `setf` updates the variable tracking the current player position and
;; `draw-known-city` is subsequently invoked.
;;
;; Next, the call to `cond` handles the various game scenarios, which are mostly
;; self-explanetory.  The most interesting case is when we run into a Glowworm,
;; in which case we randomly sample a new node, and then drop ourselves on it by
;; recursively calling `handle-new-place`.
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
	 (has-worm (and (member 'glow-worm node)
			(not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
	  ((member 'wumpus node) (if charging
				     (princ "You found the Wumpus!")
				     (princ "You ran into the Wumpus")))
	  (charging (princ "You wasted your last bullet. Game Over."))
	  (has-worm (let ((new-pos (random-node)))
		      (princ "You ran into a Glow Worm Gang! You're now at ")
		      (princ new-pos)
		      (handle-new-place nil new-pos nil))))))


;; Wrapper function that checks whether the move to destination node `pos` from
;; source node *player-pos* is a valid move.  If it is then the workhorse
;; `handle-new-place` function is invoked, or otherwise an message is printed
;; that the move was illegal and nothing further is performed.  Page 148.
;;
;; The inner call to `assoc` and subsequent `cdr` gets the list of edges that
;; are connected to *player-pos* (i.e. the current player location).  Then the
;; outer call to `assoc` returns the edge used to travel to the destination
;; node.  If the return value is nil then we know that this is not a valid move.
(defun handle-direction (pos charging)
  (let ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
	(handle-new-place edge pos charging)
	(princ "That location does not exist!"))))


;; User-facing game function, page 148.
(defun walk (pos)
  (handle-direction pos nil))


;; User-facing game function, page 148.
(defun charge (pos)
  (handle-direction pos t))
