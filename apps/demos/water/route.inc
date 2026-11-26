;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water's routes, the job a child of its farm does for the auto pilot: the
; line down the middle of a tunnel from one junction to the next, or over
; the reef. The app knows the curve a tunnel is carved round, field.shader,
; but the open water of it is moved by the warp that winds the caves, so a
; child samples the curve every unit and moves each place to the middle of
; the tunnel there: up the slope of the field across the tunnel, as the
; field is near enough the distance to the rock, so the place is as far
; from the rock as it can be. Over the reef a place is lifted over the rock
; it runs into. The line is smoothed, and the auto pilot swims along it and
; looks along it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/consts/numbers.inc")
(import "lib/gpu/vp.inc")

;module
(env-push)

;how far apart the places of a line are, how many steps a place takes to
;the middle and how far each at most, how far it looks across for the
;slope, how far over the rock a line over the reef keeps, how far it is
;lifted a step, and the highest it may go
(defq +line_step +double_1 +centre_steps 8 +centre_most +double_1/2 +slope_across +double_1/5
	+over_rock +double_4 +lift_step +double_2 +line_top (n2d 25))

(defun route-funcs ()
	; (route-funcs) -> funcs
	;the field's native functions a child makes the once and keeps
	(defq field (shader-load "apps/demos/water/field.shader"))
	(list (shader-vp-func field 'water) (shader-vp-func field 'warp)))

(defun resample (pts)
	; (resample pts) -> (doubles ...)
	;places along the line through pts, +line_step apart, and its last
	(defq out (list (first pts)))
	(each (lambda (a b)
			(defq ab (nums-sub b a) len (sqrt (nums-dot ab ab)) n (max 1 (n2i (/ len +line_step))))
			(each! (lambda () (push out (nums-add a (nums-scale ab (/ (n2d (inc (!))) (n2d n)))))) '() 0 n))
		(slice pts 0 -2) (slice pts 1 -1))
	out)

(defun smooth (pts)
	; (smooth pts) -> (doubles ...)
	;each place the mean of itself twice and its neighbours, the ends kept
	(defq out (list (first pts)))
	(each (lambda (a p b)
			(push out (nums-scale (nums-add (nums-add a b) (nums-scale p +double_2)) +double_1/4)))
		(slice pts 0 -3) (slice pts 1 -2) (slice pts 2 -1))
	(if (> (length pts) 1) (push out (last pts)))
	out)

(defun unit-way (a b)
	; (unit-way a b) -> doubles
	;the way from a to b, of length 1
	(defq d (nums-sub b a) dd (nums-dot d d))
	(if (> dd +double_0) (nums-scale d (recip (sqrt dd))) (doubles +double_0 +double_1 +double_0)))

(defun route-line (funcs pts cave)
	; (route-line funcs pts cave) -> doubles
	;the line along a tunnel through pts, a junction, the middle of its
	;curve and the next junction, moved to the middle of the open water; or
	;over the reef, lifted over the rock; its places one after another
	(bind '(water warp) funcs)
	(defq line (resample pts))
	(cond
		(cave
			;each place where the warp puts the open water, then up the slope
			;of the field across the line, the way along it taken out
			(setq line (map (# (nums-sub %0 (warp %0))) line))
			(setq line (map (lambda (p)
					(defq i (!) along (unit-way (elem-get line (max 0 (dec i))) (elem-get line (min (dec (length line)) (inc i)))))
					(times +centre_steps
						(defq h (water p) e +slope_across
							g (doubles (- (water (nums-add p (doubles e +double_0 +double_0))) h)
								(- (water (nums-add p (doubles +double_0 e +double_0))) h)
								(- (water (nums-add p (doubles +double_0 +double_0 e))) h))
							g (nums-sub g (nums-scale along (nums-dot g along))) gg (nums-dot g g))
						(if (> gg +double_0)
							(setq p (nums-add p (nums-scale g (/ (min +centre_most (+ +double_1/10 (* (abs h) +double_1/2))) (sqrt gg)))))))
					p)
				line)))
		(:t ;over the reef, lifted over the rock, and never to the surface
			(setq line (map (lambda (p)
					(defq p (cat p))
					(times 8 (if (< (water p) +over_rock) (elem-set p 1 (+ (second p) +lift_step))))
					(elem-set p 1 (min (second p) +line_top))
					p)
				line))))
	(apply (const cat) (smooth (smooth line))))

;module
(export-symbols '(route-funcs route-line resample))
(env-pop)
