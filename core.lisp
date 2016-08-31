(defparameter *nodes* '((sala (Estas en la sala. Es una cagada))
						(pieza (Estas en la pieza. Es un quilombo))
						(patio (Estas en el patio. Hay un perro bloqueando la puerta))))

(defun describe-location (location nodes)
	(cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)
									(attic upstairs ladder))
						(garden (living-rom east door))
						(attic (living-room downstairs ladder))))

(defun describe-path (edge)
	`(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
	(apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
								(bucket living-room)
								(chain garden)
								(frog garden)))

(defun objects-at (loc objs obj-locs)
	(labels ((at-loc-p (obj)
				(eq (cadr (assoc obj obj-locs)) loc)))
		(remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc obs obj-loc)
	(labels ((describe-obj (obj)
					`(you see a ,obj on the floor.)))
		(apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
	(append (describe-location *location* *nodes*)
			(describe-paths *location* *edges*)
			(describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
	(let ((next (find direction
					(cdr (assoc *location* *edges*))
					:key #'cadr)))
	(if next
		(progn (setf *location* (car next))
				(look))
		'(you cannot go that way))))

(defun pickup (object)
	(cond ((member object
				(objects-at *location* *objects* *object-locations*))
			(push (list object 'body) *object-locations*)
			`(you are now carrying the ,object))
			(t '(you cannot get that.))))

(defun inventory ()
	(cons 'items- (objects-at 'body *objects* *object-locations*)))







