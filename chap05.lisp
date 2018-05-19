;; building a text game engine, pages 67-84

;; variable specifying the scenery with an association list, page 70
(defparameter *nodes* '((living-room (you are in the living-room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))

;; describe the scenery for a given location, page 71
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; variable specifying the paths between locations with an association list,
;; page 72
(defparameter *edges* '((living-room (garden west door) (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

;; describe the path for a given path, page 73
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; describe all of the paths for a given location, page 73
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path
			  (cdr (assoc location edges)))))

;; variable specifying possible objects, page 77
(defparameter *objects* '(whiskey bucket frog chain))

;; variable specifying object-at-location pairs, page 77
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; return a list of objects visible at a given location, page 78
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; describe the objects visible at a given location, page 78
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; variable tracking the current location
(defparameter *location* 'living-room)

;; describe player's current state information, including (i) location and what
;; the player sees there, (ii) available paths to other locations, and (iii)
;; available objects at the location, page 79
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; change player's state to a new location based on `direction`, and describe
;; updated state information, page 81
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

;; pick up an object.  Note that objects aren't removed from *object-locations*,
;; but are instead hidden.  See text for more information on page 82.
(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

;; return the objects that were picked up, page 83
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
