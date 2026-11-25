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
			ridge (- +double_1 (abs (- (* +double_2 (noise (nums-scale way (const (n2d 3.1))))) +double_1)))
			lumps (+ (const (n2d 0.55)) (* (const (n2d 0.25)) (noise (nums-scale way (const (n2d 1.7)))))
				(* (const (n2d 0.1)) (noise (nums-scale way (const (n2d 4.3))))) (* (const (n2d 0.12)) ridge ridge))
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
	(list (/ (n2d (>> state 8)) (const (n2d 8388608))) state))

(defun rock-mesh (seed)
	; (rock-mesh seed) -> mesh
	;the mesh of a rock, all its numbers of its seed: where in the noise its
	;lumps are, its three axes squashed, four planes it is broken by, and
	;two bites out of it at its surface, each a ball of its own size
	(defq state seed picks (map! (lambda ()
			(bind '(n s) (rock-rnd state))
			(setq state s)
			n) '() 0 26)
		lumps_at (doubles (* (elem-get picks 0) (const (n2d 50))) (* (elem-get picks 1) (const (n2d 50)))
			(* (elem-get picks 2) (const (n2d 50))))
		squash (list (+ (const (n2d 0.75)) (* (elem-get picks 3) (const (n2d 0.4))))
			(+ (const (n2d 0.5)) (* (elem-get picks 4) (const (n2d 0.35))))
			(+ (const (n2d 0.7)) (* (elem-get picks 5) (const (n2d 0.4)))))
		bites (map (lambda (at)
				(defq a (* (elem-get picks at) +double_2pi) b (- (* (elem-get picks (inc at)) +double_2) +double_1)
					c (sqrt (- +double_1 (* b b))) out (const (n2d 0.85)))
				(list (* out c (cos a)) (* out b) (* out c (sin a))
					(+ (const (n2d 0.15)) (* (elem-get picks (+ at 2)) (const (n2d 0.2))))))
			'(6 9))
		cuts (map (lambda (at)
				(defq a (* (elem-get picks at) +double_2pi) b (- (* (elem-get picks (inc at)) +double_2) +double_1)
					c (sqrt (- +double_1 (* b b))))
				(list (* c (cos a)) b (* c (sin a)) (+ (const (n2d 0.32)) (* (elem-get picks (+ at 2)) (const (n2d 0.28))))))
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
			float (- (sqrt (+ (* px px) (* (+ py (const (n2d 0.3))) (+ py (const (n2d 0.3)))) (* pz pz))) (const (n2d 0.42)))
			ring (- (sqrt (+ (* (- r (const (n2d 0.5))) (- r (const (n2d 0.5)))) (* (+ py (const (n2d 0.3))) (+ py (const (n2d 0.3))))))
				(const (n2d 0.08)))
			mast (max (- r (const (n2d 0.06))) (- (abs (- py (const (n2d 0.35)))) (const (n2d 0.3))))
			lamp (- (sqrt (+ (* px px) (* (- py (const (n2d 0.72))) (- py (const (n2d 0.72)))) (* pz pz))) (const (n2d 0.16))))
		(buoy-smooth (buoy-smooth (buoy-smooth float ring (const (n2d 0.06))) mast (const (n2d 0.08))) lamp (const (n2d 0.05)))))

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
			body (max (- r (const (n2d 0.16))) (- (abs (+ py (const (n2d 0.2)))) (const (n2d 0.55))))
			;the nose, a long half ball, near enough a distance
			ny (/ (- py (const (n2d 0.35))) (const (n2d 0.55)))
			nose (* (- (sqrt (+ (/ (* r r) (const (n2d 0.0256))) (* ny ny))) +double_1) (const (n2d 0.16)))
			;the fins, two thin blades across each other at the tail
			tail (- (abs (+ py (const (n2d 0.6)))) (const (n2d 0.16)))
			fins (min (max (- (abs px) (const (n2d 0.05))) (- (abs pz) (const (n2d 0.4))) tail)
				(max (- (abs pz) (const (n2d 0.05))) (- (abs px) (const (n2d 0.4))) tail)))
		(buoy-smooth (buoy-smooth body nose (const (n2d 0.05))) fins (const (n2d 0.03)))))

(defun missile-mesh ()
	; (missile-mesh) -> mesh
	(Mesh-iso (Iso-missile 28) +double_0))
