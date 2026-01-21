;common imports
(import "usr/env.inc")
(import "gui/lisp.inc")

;debug options
(case :nil
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(cond   ;pick number format :t/:nil
	(:t ;reals
		(defmacro vector (&rest %0) `(reals ~%0))
		(defmacro i2n (%0) `(n2r ,%0))
		(defmacro f2n (%0) `(n2r ,%0)))
	(:t ;fixed point
		(defmacro vector (&rest %0) `(fixeds ~%0))
		(defmacro i2n (%0) `(n2f ,%0))
		(defmacro f2n (%0) %0)))

(defmacro vector-n2f (&rest %0)
	(static-qq (fixeds ~(map (lambda (%0) (list 'n2f %0)) %0))))

(defmacro vector-i2n (&rest %0)
	(static-qq (vector ~(map (lambda (%0) (list 'i2n %0)) %0))))

(defmacro vector-f2n (&rest %0)
	(static-qq (vector ~(map (lambda (%0) (list 'f2n %0)) %0))))

(enums +dlist 0
	(enum mask light_pos layer1_canvas layer1_verts))

(enums +path 0
	(enum mode color radius path))

(enums +vertex 0
	(enum p v r c))

(defq box_size 500 focal_len 1 max_vel 5 num_bubbles 50 bubble_radius 70
	light_pos (vector-i2n (neg box_size) (neg box_size) (neg (* box_size 4))))
