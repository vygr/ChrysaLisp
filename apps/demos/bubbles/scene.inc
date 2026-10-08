;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The scene of the Bubbles demo, and the message for a slice of it.
;
; It is a structure of arrays. There is not a list of bubbles, each
; with its place and its speed, there is an array of every x, one of
; every y, and so on. So a frame is not a loop over the bubbles, it is
; a few array operations, each a single call of native code whatever
; the number of bubbles. Where they all are, where they are on the
; screen and how big, is worked out without a line of Lisp per bubble.
;
; The scene is made from a seed, and a bubble's place is a function of
; the time, it bounces in its box as a triangle wave. So the app and
; every child have the same scene from four numbers, and nothing of it
; is sent.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")
(import "lib/task/jobs.inc")

(defq +width 600 +height 600
	+box_size 500.0 +max_vel 5 +bubble_radius 70.0 +base 0.3
	;how far back the box is from the eye
	+eye_dist 1005.0)

;a slice for a child to draw. The key of the shared pixels, the seed and
;the number of bubbles, the time, which way the light is, and the rows
(structure +slice +job_size
	(long shared seed count time light_x light_y)
	(uint y y1))

;its answer, the first row, and how many bubbles it drew, -1 if it could
;not reach the pixels
(structure +slice_reply +job_reply_size
	(uint y drawn))

(defun scene-fill (count val)
	;an array of that many of a value
	(apply (const fixeds) (map (lambda (_) val) (range 0 count))))

(defun scene-make (seed count)
	; (scene-make seed count) -> scene
	;the bubbles, from a seed, the same on every node. Each of these is an
	;array with an element for every bubble
	;(px py pz vx vy vz rad cr cg cb box box2 box4 eye hsw hsh one)
	(defq state seed palette '((0.0 1.0 1.0) (1.0 1.0 0.0) (1.0 0.0 1.0)
		(1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0)))
	(defun rnd (range)
		;the next number of a sequence that the seed fixes, 0 up to range
		(setq state (logand (+ (* state 6364136223846793005) 1442695040888963407) -1))
		(% (logand (>> state 33) 0x3fffffff) range))
	(defq px (list) py (list) pz (list) vx (list) vy (list) vz (list)
		cr (list) cg (list) cb (list))
	(times count
		(push px (n2f (- (rnd 1000) 500)))
		(push py (n2f (- (rnd 1000) 500)))
		(push pz (n2f (- (rnd 1000) 500)))
		(push vx (n2f (- (rnd (const (inc (* +max_vel 2)))) +max_vel)))
		(push vy (n2f (- (rnd (const (inc (* +max_vel 2)))) +max_vel)))
		(push vz (n2f (- (rnd (const (inc (* +max_vel 2)))) +max_vel)))
		(bind '(r g b) (elem-get palette (rnd (length palette))))
		(defq bright (* (/ (n2f (rnd 1000)) 1000.0) (const (- 1.0 +base))))
		(push cr (+ +base (* r bright)))
		(push cg (+ +base (* g bright)))
		(push cb (+ +base (* b bright))))
	(cat (map (# (apply (const fixeds) %0)) (list px py pz vx vy vz))
		(list (scene-fill count +bubble_radius))
		(map (# (apply (const fixeds) %0)) (list cr cg cb))
		(map (# (scene-fill count %0)) (list +box_size (const (* +box_size 2.0)) (const (* +box_size 4.0))
			+eye_dist (const (n2f (>> +width 1))) (const (n2f (>> +height 1))) 1.0))))

(defun scene-bounce (p0 v time box box2 box4)
	;where every bubble is on one axis at a time. It goes on at its speed
	;and the box folds it back, a triangle wave, five array operations
	(defq p (nums-scale v time))
	(nums-add p p0 p)
	(nums-add p box p)
	(nums-mod p box4 p)
	(nums-add p box4 p)
	(nums-mod p box4 p)
	(nums-sub p box2 p)
	(nums-abs p p)
	(nums-sub box p p))

(defun scene-frame (scene time light_x light_y)
	; (scene-frame scene time light_x light_y) -> (sx sy sr hx hy z)
	;every bubble as it is on the screen at a time, an array each. Where
	;it is, how big, where its highlight is, and how far away
	(bind '(px py pz vx vy vz rad cr cg cb box box2 box4 eye hsw hsh one) scene)
	(defq time (n2f time)
		x (scene-bounce px vx time box box2 box4)
		y (scene-bounce py vy time box box2 box4)
		z (nums-add (scene-bounce pz vz time box box2 box4) eye)
		;things get smaller with distance
		h (nums-div hsh z)
		sr (nums-mul rad h)
		;the highlight is a radius away, toward the light
		hx (nums-add (nums-mul (nums-add x (nums-scale rad light_x)) h) hsw)
		hy (nums-add (nums-mul (nums-add y (nums-scale rad light_y)) h) hsh))
	(nums-add (nums-mul x h x) hsw x)
	(nums-add (nums-mul y h y) hsh y)
	(list x y sr hx hy z))

(defun circle (r)
	;cached circle generation, quantised to 1/4 pixel
	(defq r (* (floor (* r 4.0)) 0.25))
	(memoize r (path-gen-arc 0.0 0.0 0.0 +fp_2pi r (path)) 13))

(defun scene-color (r g b at)
	;a colour, dimmed with distance, see through
	(+ 0xd0000000
		(<< (min 255 (n2i (* r at 255.0))) 16)
		(<< (min 255 (n2i (* g at 255.0))) 8)
		(min 255 (n2i (* b at 255.0)))))

(defun scene-draw (canvas scene time light_x light_y y y1)
	; (scene-draw canvas scene time light_x light_y y y1) -> drawn
	;the rows of the scene from y up to y1, how many bubbles were drawn is
	;the result. The canvas is taken to be clear there. The bubbles that
	;reach the rows are found, sorted far to near, and drawn, and the clip
	;of the canvas cuts them to the rows.
	(bind '(sx sy sr hx hy z) (scene-frame scene time light_x light_y))
	(bind '(& & & & & & & cr cg cb &rest _) scene)
	(defq fy (n2f y) fy1 (n2f y1) near (list))
	;the broad phase, the one loop over every bubble there is. A highlight
	;sits on the edge of its bubble and so reaches a little past it
	(each (lambda (by r)
		(setq r (* r 1.25))
		(if (and (> (+ by r) fy) (< (- by r) fy1)) (push near (!)))) sy sr)
	;far to near. Two as far away as each other go by their number, so
	;that every slice draws them in the one order
	(sort near (lambda (a b)
		(defq za (elem-get z a) zb (elem-get z b))
		(cond ((< za zb) 1) ((> za zb) -1) ((- a b)))))
	(. canvas :set_clip 0 y +width y1)
	(each (lambda (i)
		(defq r (elem-get sr i) at (/ +box_size (elem-get z i)))
		(.-> canvas
			(:set_color (scene-color (elem-get cr i) (elem-get cg i) (elem-get cb i) at))
			(:fpoly (elem-get sx i) (elem-get sy i) +winding_odd_even (list (circle r)))
			(:set_color (scene-color 1.0 1.0 1.0 at))
			(:fpoly (elem-get hx i) (elem-get hy i) +winding_odd_even (list (circle (* r 0.2))))))
		near)
	(length near))
