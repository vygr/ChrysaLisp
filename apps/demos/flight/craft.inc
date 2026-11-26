;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The craft, the parts they are made of and the farm that makes them.
;
; A part is a field, a near distance to its surface in the cube -1 to
; 1, less than 0 inside, made a mesh by (Mesh-iso) at 0, its corners
; smooth, as the shaders want it, (mesh-corners-smooth). The app gives
; a child of its farm a part to make, its number and the form that
; makes it, and the child sends back the mesh. Fields are joined by a
; smooth least, so a dome grows out of a hull and does not sit on it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")
(import "lib/math/surface.inc")

;a part for a child to make, its number, whether its faces are lit flat,
;and the form that makes it
(structure +part +job_size
	(uint num flat)
	(offset command))

;its answer, the mesh as the shaders want it
(structure +part_reply +job_reply_size
	(uint num)
	(offset data))

(enums +craft 0
	(enum saucer ring lattice ball blob shard delta bell top cow))

;module
(env-push)

;the numbers of the parts' shapes. The saucer's hull, how wide and how
;thick, its rim, where, how far down and how thick, its dome, and how they
;run together; cos 8a as a polynomial of cos a, its coefficients; the
;ring, where round, how thin and how thick its lobes; the lattice, where
;its shell is, how thick, and how wide its gaps; the ball, squashed and
;its size; a shard's corner; the triangle, the side of its edge, where
;its edge is, its hull's rise to the middle, its deck in, up and thick,
;its ridges from and to, thick and run on, and its dome squashed; the
;bell, squashed, where its shell is and how thick; the top, its cones'
;slope, where and their scale, its band, and its knob, where and how big;
;the cow, long, high and wide, its head, where, up, how big and run on,
;and its legs down and thick; and the blob, squashed
(defq +saucer_wide (n2d 0.92) +saucer_thick (n2d 0.17) +saucer_rim_at (n2d 0.8)
	+saucer_rim_drop (n2d 0.02) +saucer_rim_thick (n2d 0.09) +saucer_dome (n2d 0.33)
	+saucer_rim_blend (n2d 0.08) +saucer_dome_blend (n2d 0.14)
	+cos8_c2 (n2d -32) +cos8_c4 (n2d 160) +cos8_c6 (n2d -256) +cos8_c8 (n2d 128)
	+ring_at (n2d 0.86) +ring_thin (n2d 0.04) +ring_lobe (n2d 0.08)
	+lattice_at (n2d 0.78) +lattice_thick (n2d 0.07) +lattice_gaps (n2d 0.55)
	+ball_squash (n2d 0.6) +ball_size (n2d 0.9) +shard_corner (n2d 0.6) +shard_corner_neg (n2d -0.6)
	+delta_side (n2d 0.866) +delta_side_neg (n2d -0.866) +delta_edge (n2d 0.42)
	+delta_hull_rise (n2d 0.08) +delta_deck_in (n2d 0.09) +delta_deck_up (n2d 0.04)
	+delta_deck_thick (n2d 0.06) +ridge_from (n2d 0.12) +ridge_to (n2d 0.03) +ridge_thick (n2d 0.035)
	+ridge_blend (n2d 0.03) +delta_dome_squash (n2d 1.4)
	+bell_squash (n2d 0.85) +bell_at (n2d 0.72) +bell_thick (n2d 0.07)
	+top_slope (n2d 1.25) +top_cone (n2d 0.82) +top_cone_scale (n2d 0.62) +top_band_at (n2d 0.8)
	+top_knob_at (n2d 0.64) +top_knob (n2d 0.09)
	+cow_long (n2d 0.55) +cow_high (n2d 0.3) +cow_wide (n2d 0.27) +cow_head_at (n2d 0.62)
	+cow_head_up (n2d 0.16) +cow_head (n2d 0.15) +cow_head_blend (n2d 0.08) +cow_leg_down (n2d -0.62)
	+cow_leg_thick (n2d 0.06) +blob_squash (n2d 1.3))

(defun smooth-min (a b k)
	; (smooth-min a b k) -> double
	;the least of two distances, a and b, blended over k where they meet
	(defq h (max +double_0 (min +double_1 (+ +double_1/2 (/ (* +double_1/2 (- b a)) k)))))
	(- (+ b (* (- a b) h)) (* k h (- +double_1 h))))

(defun len2 (a b)
	; (len2 a b) -> double
	(sqrt (+ (* a a) (* b b))))

(defun len3 (a b c)
	; (len3 a b c) -> double
	(sqrt (+ (* a a) (* b b) (* c c))))

(defclass Iso-part (size) (Iso size size size)
	; (Iso-part size) -> iso
	;a field of size cubed samples, each the place it is at in the cube
	;-1 to 1, which the field of a part is had of, (:field x y z)
	(defq k (/ +double_2 (n2d size)) c (* (n2d (dec size)) +double_1/2))
	(def this :points (map! (# (* (- (n2d (!)) c) k)) '() 0 size))

	(defabstractmethod :field (this x y z))
	; (. iso :field x y z) -> double

	(defmethod :get_scalar (x y z)
		; (. iso :get_scalar x y z) -> scalar
		(raise :points)
		(. this :field (elem-get points x) (elem-get points y) (elem-get points z))))

(defclass Iso-saucer (size) (Iso-part size)
	; (Iso-saucer size) -> iso
	;a hull flat and round, a rim round its edge, and a dome on top

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(defq r (len2 x z)
			hull (* (- (len3 (/ x +saucer_wide) (/ y +saucer_thick) (/ z +saucer_wide))
				+double_1) +saucer_thick)
			rim (- (len2 (- r +saucer_rim_at) (+ y +saucer_rim_drop)) +saucer_rim_thick)
			dome (- (len3 x (- y +double_1/10) z) +saucer_dome))
		(smooth-min (smooth-min hull rim +saucer_rim_blend) dome +saucer_dome_blend)))

(defun lobes8 (x z)
	; (lobes8 x z) -> double
	;the cosine of eight times the angle round, of the cosine of it, no angle
	(defq c (/ x (+ (len2 x z) (const (/ +double_1 (n2d 1000000))))) c2 (* c c))
	(+ +double_1 (* c2 (+ +cos8_c2 (* c2 (+ +cos8_c4
		(* c2 (+ +cos8_c6 (* c2 +cos8_c8)))))))))

(defclass Iso-ring (size) (Iso-part size)
	; (Iso-ring size) -> iso
	;a ring, thick in eight places round it, blobs, with the rim of what it
	;is round seen between them

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(defq lobe (max +double_0 (lobes8 x z)))
		(- (len2 (- (len2 x z) +ring_at) y) (+ +ring_thin (* +ring_lobe lobe lobe)))))

(defclass Iso-lattice (size) (Iso-part size)
	; (Iso-lattice size) -> iso
	;a shell of a ball, cut through by a gyroid, so it is a lattice that
	;is seen into

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(defq shell (- (abs (- (len3 x y z) +lattice_at)) +lattice_thick)
			kx (* x +double_7) ky (* y +double_7) kz (* z +double_7)
			g (+ (* (sin kx) (cos ky)) (* (sin ky) (cos kz)) (* (sin kz) (cos kx))))
		(max shell (* (- (abs g) +lattice_gaps) +double_1/10))))

(defclass Iso-ball (size) (Iso-part size)
	; (Iso-ball size) -> iso
	;a ball, flattened to an egg, a light or a core

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(* (- (len3 x (/ y +ball_squash) z) +ball_size) +ball_squash)))

(defclass Mesh-shard () (Mesh)
	; (Mesh-shard) -> mesh
	;a tetrahedron, four faces and no more, not of a field: what makes it is
	;its shader. Its corners a cube's every other one, each face's in the
	;order that has its normal point out
	(defq k +shard_corner n +shard_corner_neg)
	(def this :verts (doubles k k k +double_1 k n n +double_1 n k n +double_1 n n k +double_1)
		:tris (nums 1 3 2 0 0 2 3 0 0 3 1 0 0 1 2 0))
	(. this :normals))

(defun to-segment (x y z ax ay az bx by bz)
	; (to-segment x y z ax ay az bx by bz) -> double
	;how far a place is from the line from a to b
	(defq ux (- bx ax) uy (- by ay) uz (- bz az) px (- x ax) py (- y ay) pz (- z az)
		t (max +double_0 (min +double_1 (/ (+ (* px ux) (* py uy) (* pz uz)) (+ (* ux ux) (* uy uy) (* uz uz))))))
	(len3 (- px (* ux t)) (- py (* uy t)) (- pz (* uz t))))

;the corners of a triangle, its nose ahead
(defq delta_corners (list
	(list (n2d 0.0) (n2d 0.76))
	(list (n2d 0.66) (n2d -0.38))
	(list (n2d -0.66) (n2d -0.38))))

(defclass Iso-delta (size) (Iso-part size)
	; (Iso-delta size) -> iso
	;a triangle, its nose ahead, thicker to its middle, a step round its
	;edge, a dome, and a ridge from the dome to each corner

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(defq edge (- (max (neg z) (+ (* x +delta_side_neg) (* z +double_1/2))
				(+ (* x +delta_side) (* z +double_1/2))) +delta_edge)
			in (max +double_0 (min +double_1 (/ (neg edge) +delta_edge)))
			;the hull, and a deck on it a step in from the edge
			hull (max edge (- (abs y) (+ +double_1/20 (* +delta_hull_rise in))))
			deck (max (+ edge +delta_deck_in) (- (abs (- y +delta_deck_up)) (+ +delta_deck_thick (* +double_1/10 in))))
			d (min hull deck))
		(each (lambda ((cx cz))
				(setq d (smooth-min d (- (to-segment x y z +double_0 +ridge_from +double_0 cx +ridge_to cz)
					+ridge_thick) +ridge_blend)))
			delta_corners)
		(smooth-min d (- (len3 x (* (- y +double_1/10) +delta_dome_squash) z) +double_1/5) +double_1/20)))

(defclass Iso-bell (size) (Iso-part size)
	; (Iso-bell size) -> iso
	;the bell of a jellyfish, a shell of a dome, its hem in eight waves

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(max (- (abs (- (len3 x (/ y +bell_squash) z) +bell_at)) +bell_thick)
			(neg (+ y +double_1/10 (* +double_1/20 (lobes8 x z)))))))

(defclass Iso-top (size) (Iso-part size)
	; (Iso-top size) -> iso
	;two cones point to point, a band round where they meet, and a knob

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(defq r (len2 x z))
		(smooth-min (smooth-min (* (- (+ r (* (abs y) +top_slope)) +top_cone) +top_cone_scale)
				(- (len2 (- r +top_band_at) y) +double_1/20) +double_1/20)
			(- (len3 x (- y +top_knob_at) z) +top_knob) +double_1/20)))

;where a cow's legs are, across and along
(defq cow_legs (list
	(list (n2d 0.32) (n2d 0.15)) (list (n2d 0.32) (n2d -0.15))
	(list (n2d -0.32) (n2d 0.15)) (list (n2d -0.32) (n2d -0.15))))

(defclass Iso-cow (size) (Iso-part size)
	; (Iso-cow size) -> iso
	;a cow: a body, a head ahead of it, and four legs, run together, for a
	;craft to take up in its beam

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(defq body (* (- (len3 (/ x +cow_long) (/ (- y +double_1/20) +cow_high)
					(/ z +cow_wide)) +double_1) +cow_wide)
			head (- (len3 (- x +cow_head_at) (- y +cow_head_up) z) +cow_head)
			d (smooth-min body head +cow_head_blend))
		(each (lambda ((lx lz))
				(setq d (smooth-min d (- (to-segment x y z lx +double_-1/20 lz lx +cow_leg_down lz)
					+cow_leg_thick) +double_1/20)))
			cow_legs)
		d))

;the balls of a blob, where each is and how big
(defq blob_balls (list
	(list (n2d 0.55) (n2d 0.1) (n2d 0.0) (n2d 0.3))
	(list (n2d -0.3) (n2d 0.05) (n2d 0.5) (n2d 0.28))
	(list (n2d -0.3) (n2d -0.1) (n2d -0.5) (n2d 0.3))
	(list (n2d 0.0) (n2d 0.45) (n2d 0.0) (n2d 0.22))))

(defclass Iso-blob (size) (Iso-part size)
	; (Iso-blob size) -> iso
	;balls that have run into each other, a craft that is grown, not made

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(defq d (- (len3 x (* y +blob_squash) z) +double_1/2))
		(each (lambda ((bx by bz br))
				(setq d (smooth-min d (- (len3 (- x bx) (- y by) (- z bz)) br) +double_1/4)))
			blob_balls)
		d))

;the parts, each the form that makes it and whether its faces are lit
;flat, a shard's, or smooth
(defq craft_parts '(
	("(Mesh-iso (Iso-saucer 24) +double_0)" :nil)
	("(Mesh-iso (Iso-ring 32) +double_0)" :nil)
	("(Mesh-iso (Iso-lattice 28) +double_0)" :nil)
	("(Mesh-iso (Iso-ball 8) +double_0)" :nil)
	("(Mesh-iso (Iso-blob 20) +double_0)" :nil)
	("(Mesh-shard)" :t)
	("(Mesh-iso (Iso-delta 32) +double_0)" :nil)
	("(Mesh-iso (Iso-bell 22) +double_0)" :nil)
	("(Mesh-iso (Iso-top 20) +double_0)" :nil)
	("(Mesh-iso (Iso-cow 24) +double_0)" :nil)))

;module, the parts and their classes, which the children make by name,
;and what the classes read as they run: their numbers, prebound, go with
;it
(export-symbols '(craft_parts delta_corners cow_legs blob_balls))
(export-classes '(Iso-part Iso-saucer Iso-ring Iso-lattice Iso-ball Mesh-shard Iso-delta
	Iso-bell Iso-top Iso-cow Iso-blob))
(env-pop)
