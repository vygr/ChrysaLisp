;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/debug/loops.inc - what holds what is above it
;
; A thing is let go of when the last count of it goes. What holds itself
; round, a child that holds its owner, is never let go of: when the last
; outside count goes the round is left, and all it holds, held on the node
; for ever. Ownership is a tree, and a link back to an owner is a weak ref,
; claimed where it is used. (loops obj) finds each counted link back.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun loops (root &optional kids)
	; (loops obj [kids]) -> ((holder key) ...)
	;each place under obj that holds obj or something between it and
	;obj: the holder, an environment or a list, and the key or the index
	;it is at. What is under a thing is its properties, if it is an
	;environment, its elements, if it is a list, and what kids gives of it,
	;(kids thing) -> things, the children of a view say. A function is not
	;looked in, nor what was looked in already by another way down
	(defq out (list) seen (Fset 101) todo (list (list root (list root))))
	(. seen :insert (weak-ref root))
	(while (defq next (pop todo))
		(bind '(thing up) next)
		(defq links (cond
				((env? thing)
					(filter (# (not (eql (first %0) :vtable))) (tolist thing)))
				((and (list? thing) (not (func? thing)) (not (lambda? thing)))
					(map (# (list (!) %0)) thing))
				((list))))
		(if kids (each (# (push links (list :kid %0))) (ifn (kids thing) (list))))
		(each (lambda ((key val))
				(cond
					((not (or (env? val) (list? val))))
					((func? val))
					((some (# (if (eql %0 val) :t)) up) (push out (list thing key)))
					((. seen :find (weak-ref val)))
					(:t (. seen :insert (weak-ref val))
						(push todo (list val (cat up (list val)))))))
			links))
	out)

(defun view-loops (root)
	; (view-loops view) -> ((holder key) ...)
	;(loops) of a tree of views, a view's children under it. An app's
	;window should have none
	(loops root (# (if (View? %0) (. %0 :children)))))

;module
(export-symbols '(loops view-loops))
(env-pop)
