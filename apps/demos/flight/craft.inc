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
			hull (* (- (len3 (/ x (const (n2d 0.92))) (/ y (const (n2d 0.17))) (/ z (const (n2d 0.92))))
				+double_1) (const (n2d 0.17)))
			rim (- (len2 (- r (const (n2d 0.8))) (+ y (const (n2d 0.02)))) (const (n2d 0.09)))
			dome (- (len3 x (- y (const (n2d 0.1))) z) (const (n2d 0.33))))
		(smooth-min (smooth-min hull rim (const (n2d 0.08))) dome (const (n2d 0.14)))))

(defclass Iso-ring (size) (Iso-part size)
	; (Iso-ring size) -> iso
	;a ring, thick in eight places round it, blobs, with the rim of what it
	;is round seen between them

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		;the cosine of eight times the angle round, of the cosine of it
		(defq r (+ (len2 x z) (const (/ +double_1 (n2d 1000000)))) c (/ x r) c2 (* c c)
			lobes (+ +double_1 (* c2 (+ (const (n2d -32)) (* c2 (+ (const (n2d 160))
				(* c2 (+ (const (n2d -256)) (* c2 (const (n2d 128))))))))))
			lobe (max +double_0 lobes)
			thick (+ (const (n2d 0.04)) (* (const (n2d 0.08)) lobe lobe)))
		(- (len2 (- r (const (n2d 0.86))) y) thick)))

(defclass Iso-lattice (size) (Iso-part size)
	; (Iso-lattice size) -> iso
	;a shell of a ball, cut through by a gyroid, so it is a lattice that
	;is seen into

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(defq shell (- (abs (- (len3 x y z) (const (n2d 0.78)))) (const (n2d 0.07)))
			kx (* x (const (n2d 7))) ky (* y (const (n2d 7))) kz (* z (const (n2d 7)))
			g (+ (* (sin kx) (cos ky)) (* (sin ky) (cos kz)) (* (sin kz) (cos kx))))
		(max shell (* (- (abs g) (const (n2d 0.55))) (const (n2d 0.1))))))

(defclass Iso-ball (size) (Iso-part size)
	; (Iso-ball size) -> iso
	;a ball, flattened to an egg, a light or a core

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(* (- (len3 x (/ y (const (n2d 0.6))) z) (const (n2d 0.9))) (const (n2d 0.6)))))

(defclass Mesh-shard () (Mesh)
	; (Mesh-shard) -> mesh
	;a tetrahedron, four faces and no more, not of a field: what makes it is
	;its shader. Its corners a cube's every other one, each face's in the
	;order that has its normal point out
	(defq k (const (n2d 0.6)) n (const (n2d -0.6)))
	(def this :verts (doubles k k k +double_1 k n n +double_1 n k n +double_1 n n k +double_1)
		:tris (nums 1 3 2 0 0 2 3 0 0 3 1 0 0 1 2 0))
	(. this :normals))

(defun lobes8 (x z)
	; (lobes8 x z) -> double
	;the cosine of eight times the angle round, of the cosine of it, no angle
	(defq c (/ x (+ (len2 x z) (const (/ +double_1 (n2d 1000000))))) c2 (* c c))
	(+ +double_1 (* c2 (+ (const (n2d -32)) (* c2 (+ (const (n2d 160))
		(* c2 (+ (const (n2d -256)) (* c2 (const (n2d 128)))))))))))

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
		(defq edge (- (max (neg z) (+ (* x (const (n2d -0.866))) (* z +double_1/2))
				(+ (* x (const (n2d 0.866))) (* z +double_1/2))) (const (n2d 0.42)))
			in (max +double_0 (min +double_1 (/ (neg edge) (const (n2d 0.42)))))
			;the hull, and a deck on it a step in from the edge
			hull (max edge (- (abs y) (+ (const (n2d 0.05)) (* (const (n2d 0.08)) in))))
			deck (max (+ edge (const (n2d 0.09))) (- (abs (- y (const (n2d 0.04)))) (+ (const (n2d 0.06)) (* (const (n2d 0.1)) in))))
			d (min hull deck))
		(each (lambda ((cx cz))
				(setq d (smooth-min d (- (to-segment x y z +double_0 (const (n2d 0.12)) +double_0 cx (const (n2d 0.03)) cz)
					(const (n2d 0.035))) (const (n2d 0.03)))))
			delta_corners)
		(smooth-min d (- (len3 x (* (- y (const (n2d 0.1))) (const (n2d 1.4))) z) (const (n2d 0.2))) (const (n2d 0.05)))))

(defclass Iso-bell (size) (Iso-part size)
	; (Iso-bell size) -> iso
	;the bell of a jellyfish, a shell of a dome, its hem in eight waves

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(max (- (abs (- (len3 x (/ y (const (n2d 0.85))) z) (const (n2d 0.72)))) (const (n2d 0.07)))
			(neg (+ y (const (n2d 0.1)) (* (const (n2d 0.05)) (lobes8 x z)))))))

(defclass Iso-top (size) (Iso-part size)
	; (Iso-top size) -> iso
	;two cones point to point, a band round where they meet, and a knob

	(defmethod :field (x y z)
		; (. iso :field x y z) -> double
		(defq r (len2 x z))
		(smooth-min (smooth-min (* (- (+ r (* (abs y) (const (n2d 1.25)))) (const (n2d 0.82))) (const (n2d 0.62)))
				(- (len2 (- r (const (n2d 0.8))) y) (const (n2d 0.05))) (const (n2d 0.05)))
			(- (len3 x (- y (const (n2d 0.64))) z) (const (n2d 0.09))) (const (n2d 0.05)))))

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
		(defq body (* (- (len3 (/ x (const (n2d 0.55))) (/ (- y (const (n2d 0.05))) (const (n2d 0.3)))
					(/ z (const (n2d 0.27)))) +double_1) (const (n2d 0.27)))
			head (- (len3 (- x (const (n2d 0.62))) (- y (const (n2d 0.16))) z) (const (n2d 0.15)))
			d (smooth-min body head (const (n2d 0.08))))
		(each (lambda ((lx lz))
				(setq d (smooth-min d (- (to-segment x y z lx (const (n2d -0.05)) lz lx (const (n2d -0.62)) lz)
					(const (n2d 0.06))) (const (n2d 0.05)))))
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
		(defq d (- (len3 x (* y (const (n2d 1.3))) z) (const (n2d 0.5))))
		(each (lambda ((bx by bz br))
				(setq d (smooth-min d (- (len3 (- x bx) (- y by) (- z bz)) br) (const (n2d 0.25)))))
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
(enums +craft 0
	(enum saucer ring lattice ball blob shard delta bell top cow))
