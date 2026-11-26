;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, how the water looks, functions for a shader to import: its
; colour seen from a depth one way, the net of light the ripples of
; the surface throw down, the caustics, and the surface seen from
; under it, the window of the sky straight up and the sun in it.
; No inputs, no main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/water/field.shader")

;the water deep down and near the surface, the sky over it, and the
;sunlight
(defconst water_deep (vec3 0.0 0.06 0.1))
(defconst water_shallow (vec3 0.1 0.56 0.62))
(defconst sky_over (vec3 0.55 0.8 0.95))
(defconst sun_light (vec3 1.0 0.94 0.78))

;how much of what is the way of to from the eye, and that far, is seen
;through the water, 0 to 1: the water is clear up and down, as in the
;shallows of the tropics, and murky across, a way up or down counted a
;third as far as a way across
(defconst clear_up 0.12)
(defun seen_through :float ((to :vec3) (murk :float))
	(exp (* (- 0.0 murk) (sqrt (+ (dot (:xz to) (:xz to)) (* clear_up (:y to) (:y to)))))))

;the colour of the water seen from a height y, looking the way of dy up:
;bright near the surface and looking up, dark deep down and looking down.
;A shader makes it darker still where the eye is shut in, in the caves
(defun water_colour :vec3 ((y :float) (dy :float))
	(defq depth (clamp (/ (- surface y) 160.0) 0.0 1.0)
		k (+ (* 0.65 (- 1.0 depth)) (* 0.35 (clamp (+ (* dy 0.5) 0.5) 0.0 1.0))))
	(mix water_deep water_shallow (* k k)))

;the caustics at a place, 0 to 1: two layers of noise drifting apart,
;bright in a web of soft lines where they meet, as the ripples overhead
;focus the sun; blurred to an even shimmer by soft, 0 to 1, the further
;and deeper. Their sizes are whole times round the world, field.shader
(defun caustic :float ((p :vec2) (t :float) (soft :float))
	(defq q (* p 0.34375)
		a (noise2 (+ q (vec2 (* t 0.21) (* t 0.13))))
		b (noise2 (+ (* q 1.5) (vec2 (* t -0.17) (* t 0.19)) 3.0))
		c (clamp (- 1.0 (* 2.4 (abs (- a b)))) 0.0 1.0)
		e (noise2 (+ (* q 2.25) (vec2 (* t 0.11) (* t -0.23)) 7.0))
		f (clamp (- 1.0 (* 2.8 (abs (- b e)))) 0.0 1.0))
	(mix (+ (* c c c) (* 0.5 f f f)) 0.3 soft))

;how high the little bumps of the reef are at a place: ripples on the
;sand, wandering, with grain over them; rough lumps on the rock; sand how
;much of it is sand. Close up, close 1, the roughness is finer: the big
;lumps fade back and fine pits and grit take over, finer the closer, so a
;wall right before the eye is finely rough, its broad shape seen further
(defun bumps :float ((p :vec3) (sand :float) (close :float))
	(defq wave (sin (+ (* (:x p) 1.9) (* (:z p) 0.7) (* 6.0 (noise2 (* (:xz p) 0.0625)))))
		grain (noise3 (* p 6.0))
		lumps (+ (* 0.7 (noise3 (* p 1.25))) (* 0.3 (noise3 (* p 3.5))))
		fine (* close (+ (* 0.14 (noise3 (* p 8.0))) (* 0.08 (noise3 (* p 19.0))) (* 0.045 (noise3 (* p 41.0))))))
	(+ (* (mix lumps (+ (* 0.35 wave) (* 0.15 grain)) sand) (mix 1.0 0.3 close)) fine))

;the normal n tilted by the slope of the bumps at p, by k, the slope
;looked for nearer close up, for the fine bumps
(defun bumped :vec3 ((p :vec3) (n :vec3) (sand :float) (k :float) (close :float))
	(defq e (mix 0.08 0.01 close) h (bumps p sand close)
		g (/ (vec3 (- (bumps (+ p (vec3 e 0.0 0.0)) sand close) h) (- (bumps (+ p (vec3 0.0 e 0.0)) sand close) h)
			(- (bumps (+ p (vec3 0.0 0.0 e)) sand close) h)) e))
	(normalize (- n (* (- g (* n (dot g n))) k))))

;the surface seen from under it, the way of d from the eye: the sky in
;its window straight up, rippled, the sun in it, and beyond the window
;the light of the water, all as dim as the water is deep between
(defun seen_surface :vec3 ((eye :vec3) (d :vec3) (t :float) (murk :float))
	(defq to (/ (- surface (:y eye)) (max (:y d) 0.02)) hit (+ (:xz eye) (* (:xz d) to))
		rip (noise2 (+ (* hit 0.25) (vec2 (* t 0.3) (* t 0.2))))
		window (ease 0.62 0.78 (:y d))
		glare (pow (max (dot d (normalize (+ sun_way (vec3 0.0 0.7 0.0)))) 0.0) 80.0)
		sky (* sky_over (* window (+ 0.75 (* 0.5 rip))))
		beyond (* water_shallow (* (- 1.0 window) (+ 0.45 (* 0.4 rip))))
		surf (+ sky beyond (* sun_light (* glare 4.0 window))))
	(mix (water_colour (:y eye) (:y d)) surf (seen_through (* d to) murk)))

;the lamp's light the water throws back toward the eye, looking the way of
;d, a glow down the beam, k how much water there is between, 0 to 1
(defun lamp_glow :vec3 ((d :vec3) (forward :vec3) (lamp :float) (k :float))
	(* (vec3 1.0 0.95 0.85) (* lamp 0.025 k (ease 0.5 1.0 (dot d forward)))))

;the water seen the way of d from the eye, as far as it goes: the surface
;looking up, the water's colour any other way, as lit as the eye is, open.
;What is far away fades into just this, so the reef has no edge
(defun water_seen :vec3 ((eye :vec3) (d :vec3) (t :float) (murk :float) (open :float))
	(if (> (:y d) 0.02)
		(* (seen_surface eye d t murk) open)
		(* (water_colour (:y eye) (:y d)) open)))
