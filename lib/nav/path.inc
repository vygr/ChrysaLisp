;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; paths: a list of places, vec3 doubles, one after another, the way a
; thing goes. Made even, places so far apart; smoothed, each the mean of
; itself and its neighbours; the way along one at a place; and how far on
; along it a thing is, a cursor that only goes on, and the place so far
; ahead of it, what a follower steers at
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/consts/numbers.inc")

;module
(env-push)

(defun nav-path-unit (v)
	; (nav-path-unit v) -> doubles
	;a way of length 1; up, of none
	(if (> (defq vv (nums-dot v v)) +double_0)
		(nums-scale v (recip (sqrt vv)))
		(doubles +double_0 +double_1 +double_0)))

(defun nav-path-resample (pts step)
	; (nav-path-resample pts step) -> (doubles ...)
	;places along the path through pts, no further apart than step, its
	;first and its last kept
	(defq out (list (first pts)))
	(each (lambda (a b)
			(defq ab (nums-sub b a) n (max 1 (n2i (ceil (/ (sqrt (nums-dot ab ab)) step)))))
			(each! (lambda () (push out (nums-add a (nums-scale ab (/ (n2d (inc (!))) (n2d n)))))) '() 0 n))
		(slice pts 0 -2) (slice pts 1 -1))
	out)

(defun nav-path-smooth (pts &optional times_of)
	; (nav-path-smooth pts [times_of]) -> (doubles ...)
	;each place the mean of itself twice and its neighbours, the ends kept,
	;so many times, once if not said
	(times (ifn times_of 1)
		(when (> (length pts) 2)
			(defq out (list (first pts)))
			(each (lambda (a p b)
					(push out (nums-scale (nums-add (nums-add a b) (nums-scale p +double_2)) +double_1/4)))
				(slice pts 0 -3) (slice pts 1 -2) (slice pts 2 -1))
			(setq pts (push out (last pts)))))
	pts)

(defun nav-path-way-at (pts i)
	; (nav-path-way-at pts i) -> doubles
	;the way along a path at its place i, of length 1
	(nav-path-unit (nums-sub (elem-get pts (min (dec (length pts)) (inc i))) (elem-get pts (max 0 (dec i))))))

(defun nav-path-length (pts)
	; (nav-path-length pts) -> double
	(reduce! (lambda (n a b) (+ n (sqrt (nums-dot (defq d (nums-sub b a)) d))))
		(list (slice pts 0 -2) (slice pts 1 -1)) +double_0))

(defun nav-path-on (pts at cursor look)
	; (nav-path-on pts at cursor look) -> cursor
	;how far along a path a thing at a place is: of the places from the
	;cursor on, no more than look of them, the nearest, or the one before
	;it if it is not there yet, so the place it is past; the cursor never
	;goes back
	(defq near (nums-dot (defq d (nums-sub (elem-get pts cursor) at)) d) out cursor)
	(each! (lambda (p)
			(if (< (defq dd (nums-dot (defq d (nums-sub p at)) d)) near) (setq near dd out (!))))
		(list pts) (inc cursor) (min (length pts) (+ cursor look 1)))
	(if (and (> out cursor) (< (inc out) (length pts))
			(< (nums-dot (nums-sub at (elem-get pts out)) (nums-sub (elem-get pts (inc out)) (elem-get pts out))) +double_0))
		(dec out) out))

(defun nav-path-past (pts cursor at)
	; (nav-path-past pts cursor at) -> double
	;how far past its place cursor, along the way to the next, a place is,
	;0 to that length: with nav-path-ahead an aim that slides on as the place
	;does, not one that jumps as the cursor does
	(cond
		((< (inc cursor) (length pts))
			(defq a (elem-get pts cursor) ab (nums-sub (elem-get pts (inc cursor)) a) ll (nums-dot ab ab))
			(if (> ll +double_0)
				(* (max +double_0 (min +double_1 (/ (nums-dot (nums-sub at a) ab) ll))) (sqrt ll))
				+double_0))
		(+double_0)))

(defun nav-path-ahead (pts cursor far)
	; (nav-path-ahead pts cursor far) -> doubles
	;the place so far on along a path from its place cursor, the last if it
	;is not that long
	(defq i cursor n (length pts) out :nil)
	(while (and (not out) (< (inc i) n))
		(defq a (elem-get pts i) b (elem-get pts (inc i)) d (sqrt (nums-dot (defq ab (nums-sub b a)) ab)))
		(if (and (> d +double_0) (<= far d))
			(setq out (nums-add a (nums-scale ab (/ far d))))
			(setq far (- far d) i (inc i))))
	(ifn out (cat (elem-get pts (dec n)))))

;module
(export-symbols
	'(nav-path-unit nav-path-resample nav-path-smooth nav-path-way-at nav-path-length nav-path-on nav-path-past nav-path-ahead))
(env-pop)
