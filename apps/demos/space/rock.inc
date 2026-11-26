;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's rocks, the shapes they are and the farm that makes them.
;
; A rock is a field, a near distance to its surface in the cube -1 to 1,
; less than 0 inside, made a mesh by (Mesh-iso) at 0, its corners smooth,
; as the shaders want it, (mesh-corners-smooth). It is a ball squashed
; along its three axes, its radius each way of the noise of that way, at
; two sizes, and ridged noise over that, sharp crests and not lumps; cut
; by planes, flat faces broken off it meeting at edges, as a chunk split
; from something bigger is; and with bites out of it where balls are
; taken away, its craters. The app gives a child of its farm a
; rock to make, its number and the numbers it is made of, and the child
; sends back the mesh. Few samples each way, a rock is a few hundred
; triangles: what makes it rough close to is its shader.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")
(import "lib/math/surface.inc")
(import "lib/math/mesh.inc")
(import "lib/gpu/shader.inc")
(import "lib/gpu/vp.inc")

;a rock for a child to make, its number, and the form that makes it
(structure +rock +job_size
	(uint num)
	(offset command))

;its answer, the mesh as the shaders want it
(structure +rock_reply +job_reply_size
	(uint num)
	(offset data))

;module
(env-push)

;the numbers of the shapes: a rock's ridges, the scale of their noise
;and their height, and its lumps, how far out they are and the scales of
;their big and small noise; the range of (rock-rnd); how far apart in the
;noise rocks' lumps are; how a rock is squashed, the least of its x and z
;and their spread and the spread of its y; how far out its bites are and
;their least size, and how far out its cuts are and their spread. The
;buoy: how far down its float is and its size, its ring's thickness, its
;mast's thickness, middle and half height, where its lamp is and its size,
;and how they are run together. The missile: its radius and half length,
;where its nose starts and how long it is, the square of its radius over
;the nose's, where its tail is and its half length, the span of its fins
;and how they are run on
(defq +ridge_scale (n2d 3.1) +ridge_height (n2d 0.12) +lumps_mid (n2d 0.55) +lumps_big (n2d 1.7)
	+lumps_small (n2d 4.3) +rnd_range (n2d 8388608) +lumps_spread (n2d 50)
	+squash_x_least (n2d 0.75) +squash_z_least (n2d 0.7) +squash_spread (n2d 0.4)
	+squash_y_spread (n2d 0.35) +bite_out (n2d 0.85) +bite_least (n2d 0.15) +cut_least (n2d 0.32)
	+cut_spread (n2d 0.28) +buoy_drop (n2d 0.3) +buoy_float (n2d 0.42) +buoy_ring (n2d 0.08)
	+buoy_mast (n2d 0.06) +buoy_mast_mid (n2d 0.35) +buoy_mast_half (n2d 0.3)
	+buoy_lamp_at (n2d 0.72) +buoy_lamp (n2d 0.16) +buoy_ring_blend (n2d 0.06)
	+buoy_mast_blend (n2d 0.08) +missile_radius (n2d 0.16) +missile_half (n2d 0.55)
	+nose_at (n2d 0.35) +nose_long (n2d 0.55) +nose_r2 (n2d 0.0256) +tail_at (n2d 0.6)
	+tail_half (n2d 0.16) +fin_span (n2d 0.4) +fin_blend (n2d 0.03))

(defclass Iso-rock (size seed squash cuts bites) (Iso size size size)
	; (Iso-rock size seed squash cuts bites) -> iso
	;a field of size cubed samples of a rock: seed where in the noise its
	;lumps are, squash its three axes, cuts a list of (x y z d) of the
	;planes it is broken by, each a way out and how far, and bites a list
	;of (x y z r) of the balls taken out of it. noise is the shaders' own,
	;native, the same function the shaders have
	(defq k (/ +double_2 (n2d size)) c (* (n2d (dec size)) +double_1/2))
	(def this :points (map! (# (* (- (n2d (!)) c) k)) '() 0 size)
		:seed seed :squash squash :cuts cuts :bites bites
		:noise (shader-vp-func (shader-load "lib/gpu/shaders/noise.shader") 'noise3))

	(defmethod :get_scalar (x y z)
		; (. iso :get_scalar x y z) -> scalar
		(raise :points :seed :squash :cuts :bites :noise)
		(bind '(sx sy sz) squash)
		(defq px (elem-get points x) py (elem-get points y) pz (elem-get points z)
			qx (/ px sx) qy (/ py sy) qz (/ pz sz)
			r (sqrt (+ (* qx qx) (* qy qy) (* qz qz) (const (/ +double_1 (n2d 1000000)))))
			;the way out from the middle, its lumps big and small
			way (nums-add (doubles (/ qx r) (/ qy r) (/ qz r)) seed)
			;and ridges, noise folded at its middle, sharp where it folds
			ridge (- +double_1 (abs (- (* +double_2 (noise (nums-scale way +ridge_scale))) +double_1)))
			lumps (+ +lumps_mid (* +double_1/4 (noise (nums-scale way +lumps_big)))
				(* +double_1/10 (noise (nums-scale way +lumps_small))) (* +ridge_height ridge ridge))
			d (- r lumps))
		;each cut, all past a plane broken off
		(each (lambda ((nx ny nz cd))
				(setq d (max d (- (+ (* px nx) (* py ny) (* pz nz)) cd))))
			cuts)
		;each bite, a ball taken out
		(each (lambda ((bx by bz br))
				(defq ex (- px bx) ey (- py by) ez (- pz bz))
				(setq d (max d (- br (sqrt (+ (* ex ex) (* ey ey) (* ez ez)))))))
			bites)
		d))

(defun rock-rnd (state)
	; (rock-rnd state) -> (number state)
	;a number from 0 to 1 of a state, and the next state: the same numbers
	;of the same seed, every time and on every node
	(setq state (logand (+ (* state 1103515245) 12345) 0x7fffffff))
	(list (/ (n2d (>> state 8)) +rnd_range) state))

(defun rock-mesh (seed)
	; (rock-mesh seed) -> mesh
	;the mesh of a rock, all its numbers of its seed: where in the noise its
	;lumps are, its three axes squashed, four planes it is broken by, and
	;two bites out of it at its surface, each a ball of its own size
	(defq state seed picks (map! (lambda ()
			(bind '(n s) (rock-rnd state))
			(setq state s)
			n) '() 0 26)
		lumps_at (doubles (* (elem-get picks 0) +lumps_spread) (* (elem-get picks 1) +lumps_spread)
			(* (elem-get picks 2) +lumps_spread))
		squash (list (+ +squash_x_least (* (elem-get picks 3) +squash_spread))
			(+ +double_1/2 (* (elem-get picks 4) +squash_y_spread))
			(+ +squash_z_least (* (elem-get picks 5) +squash_spread)))
		bites (map (lambda (at)
				(defq a (* (elem-get picks at) +double_2pi) b (- (* (elem-get picks (inc at)) +double_2) +double_1)
					c (sqrt (- +double_1 (* b b))) out +bite_out)
				(list (* out c (cos a)) (* out b) (* out c (sin a))
					(+ +bite_least (* (elem-get picks (+ at 2)) +double_1/5))))
			'(6 9))
		cuts (map (lambda (at)
				(defq a (* (elem-get picks at) +double_2pi) b (- (* (elem-get picks (inc at)) +double_2) +double_1)
					c (sqrt (- +double_1 (* b b))))
				(list (* c (cos a)) b (* c (sin a)) (+ +cut_least (* (elem-get picks (+ at 2)) +cut_spread))))
			'(12 15 18 21)))
	(Mesh-iso (Iso-rock 22 lumps_at squash cuts bites) +double_0))

(defun buoy-smooth (a b k)
	; (buoy-smooth a b k) -> double
	;the least of two distances, blended over k where they meet
	(defq h (max +double_0 (min +double_1 (+ +double_1/2 (/ (* +double_1/2 (- b a)) k)))))
	(- (+ b (* (- a b) h)) (* k h (- +double_1 h))))

(defclass Iso-buoy (size) (Iso size size size)
	; (Iso-buoy size) -> iso
	;a marker buoy: a round float, a fender ring round its middle, a mast
	;up from it and a lamp on top, run together, upright along y
	(defq k (/ +double_2 (n2d size)) c (* (n2d (dec size)) +double_1/2))
	(def this :points (map! (# (* (- (n2d (!)) c) k)) '() 0 size))

	(defmethod :get_scalar (x y z)
		; (. iso :get_scalar x y z) -> scalar
		(raise :points)
		(defq px (elem-get points x) py (elem-get points y) pz (elem-get points z)
			r (sqrt (+ (* px px) (* pz pz)))
			float (- (sqrt (+ (* px px) (* (+ py +buoy_drop) (+ py +buoy_drop)) (* pz pz))) +buoy_float)
			ring (- (sqrt (+ (* (- r +double_1/2) (- r +double_1/2)) (* (+ py +buoy_drop) (+ py +buoy_drop))))
				+buoy_ring)
			mast (max (- r +buoy_mast) (- (abs (- py +buoy_mast_mid)) +buoy_mast_half))
			lamp (- (sqrt (+ (* px px) (* (- py +buoy_lamp_at) (- py +buoy_lamp_at)) (* pz pz))) +buoy_lamp))
		(buoy-smooth (buoy-smooth (buoy-smooth float ring +buoy_ring_blend) mast +buoy_mast_blend) lamp +double_1/20)))

(defun buoy-mesh ()
	; (buoy-mesh) -> mesh
	(Mesh-iso (Iso-buoy 20) +double_0))

(defclass Iso-missile (size) (Iso size size size)
	; (Iso-missile size) -> iso
	;a missile: a round body along y, a pointed nose up, and four fins at
	;its tail, run together
	(defq k (/ +double_2 (n2d size)) c (* (n2d (dec size)) +double_1/2))
	(def this :points (map! (# (* (- (n2d (!)) c) k)) '() 0 size))

	(defmethod :get_scalar (x y z)
		; (. iso :get_scalar x y z) -> scalar
		(raise :points)
		(defq px (elem-get points x) py (elem-get points y) pz (elem-get points z)
			r (sqrt (+ (* px px) (* pz pz)))
			body (max (- r +missile_radius) (- (abs (+ py +double_1/5)) +missile_half))
			;the nose, a long half ball, near enough a distance
			ny (/ (- py +nose_at) +nose_long)
			nose (* (- (sqrt (+ (/ (* r r) +nose_r2) (* ny ny))) +double_1) +missile_radius)
			;the fins, two thin blades across each other at the tail
			tail (- (abs (+ py +tail_at)) +tail_half)
			fins (min (max (- (abs px) +double_1/20) (- (abs pz) +fin_span) tail)
				(max (- (abs pz) +double_1/20) (- (abs px) +fin_span) tail)))
		(buoy-smooth (buoy-smooth body nose +double_1/20) fins +fin_blend)))

(defun missile-mesh ()
	; (missile-mesh) -> mesh
	(Mesh-iso (Iso-missile 28) +double_0))

;module, the meshes and their classes: the numbers of the shapes,
;prebound into them as they are made, go with it
(export-symbols
	'(rock-mesh buoy-mesh missile-mesh))
(export-classes '(Iso-rock Iso-buoy Iso-missile))
(env-pop)
