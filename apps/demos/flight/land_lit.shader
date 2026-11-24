;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The land, a pixel shader. It goes with land_vertex.shader.
;
; The colour of the land by how high and how steep it is, sand by
; the water, grass, rock where it is steep, snow on the tops. Its
; face is made rough by a fine noise, a bump, as the Surface demo
; has it: the noise's slope across the face tilts the face. It is
; lit by the sun, and in shadow where the land between it and the
; sun is higher than the line to the sun, softly, as the Surface
; demo's shadow. The water is the sky's colour, more of it the more
; it is seen side on, with ripples that move with time and the sun
; shining off them. And far off, all of it goes into the haze.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/flight/land.shader")

;the way to the sun, the colour of the haze and of the sky above, the
;time, how rough the land is, and how many steps to the sun a shadow
;looks, 0 for none: the app gives less of them to the CPU than the GPU
(definput sun :vec3)
(definput haze :vec3)
(definput above :vec3)
(definput eye :vec3)
(definput time :float)
(definput rough :float 0.0 0.0 1.0)
(definput shadows :int 0 0 24)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying fade :float)

(defconst sand (vec3 0.76 0.70 0.50))
(defconst grass (vec3 0.24 0.42 0.16))
(defconst moss (vec3 0.36 0.46 0.20))
(defconst rock (vec3 0.42 0.38 0.34))
(defconst snow (vec3 0.95 0.96 1.0))
(defconst deep (vec3 0.05 0.16 0.24))
(defconst sun_colour (vec3 1.0 0.94 0.80))
(defconst eps 0.15)

;the fine noise the face is made rough by
(defun grain :float ((p :vec3))
	(+ (* (noise3 (* p 0.9)) 0.6) (* (noise3 (* p 2.3)) 0.4)))

;the face tilted by the slope of the grain across it, Surface's bumpmap
(defun bumped :vec3 ((p :vec3) (n :vec3) (by :float))
	(defq grad (/ (vec3
		(- (grain (+ p (vec3 eps 0.0 0.0))) (grain (- p (vec3 eps 0.0 0.0))))
		(- (grain (+ p (vec3 0.0 eps 0.0))) (grain (- p (vec3 0.0 eps 0.0))))
		(- (grain (+ p (vec3 0.0 0.0 eps))) (grain (- p (vec3 0.0 0.0 eps))))) (* 2.0 eps)))
	(setq grad (- grad (* n (dot n grad))))
	(normalize (- n (* by grad))))

;how much of the sun gets here, 1 none in the way: steps toward the sun,
;and the least of how far over the land each is for how far it has gone,
;Surface's soft shadow over a land in place of a field
(defun sunlit :float ((p :vec3))
	;from the land's own height here, a little over it: the grid between its
	;corners is under the top of a rise, and would be in its own shadow
	(setq p (vec3 (:x p) (+ (max (:y p) (ground (:xz p))) 0.5) (:z p)))
	(defq shade 1.0 l 2.0)
	(for (i 0 24)
		(if (>= i shadows) (break))
		(defq q (+ p (* sun l)))
		(setq shade (min shade (/ (* 6.0 (- (:y q) (ground (:xz q)))) l))
			l (* l 1.35))
		(if (< shade 0.05) (break)))
	(clamp shade 0.0 1.0))

;one train of waves, its slope across and along: which way it goes, how
;fine, how fast and how high
(defun wave :vec2 ((p :vec2) (way :vec2) (fine :float) (fast :float) (high :float))
	(* way (* high fine (cos (+ (* fine (dot way p)) (* fast time))))))

;ripples, four trains of waves that move with time, as the Surface demo's
;sines, each its own way, fine, and fast, the fine ones the lower. Read
;where a slow noise has moved them to, so their crests bend and never line
;up into a lattice, and as high as a drifting noise says, patches calm and
;patches choppy, as the wind lies on water. Less of them the further off,
;where they would be finer than a pixel
(defun ripple :vec3 ((p :vec3) (far :float))
	(defq q (:xz p)
		w (+ q (* 7.0 (vec2 (noise2 (* q 0.06)) (noise2 (+ (* q 0.06) (vec2 31.0 17.0))))))
		patch (+ 0.35 (* 1.3 (noise2 (+ (* q 0.025) (vec2 (* time 0.04) (* time 0.03))))))
		slope (* patch (+ (wave w (vec2 0.96 0.28) 1.1 1.6 0.06) (wave w (vec2 -0.37 0.93) 1.7 2.1 0.04)
			(wave w (vec2 0.71 -0.71) 2.9 3.0 0.025) (wave w (vec2 -0.95 -0.31) 4.3 3.7 0.015))))
	(normalize (vec3 (- (:x (* slope far))) 1.0 (- (:y (* slope far))))))

;the sky in a way, as sky_lit.shader has it, the haze at the horizon to
;the blue above
(defun sky_in :vec3 ((d :vec3))
	(mix haze above (pow (max (:y d) 0.0) 0.6)))

(defun main :vec4 ((frag :vec2))
	;how high the land is here: near the water the land's own height at this
	;pixel, not the grid's between its corners, so the shore follows the
	;land and not the grid's triangles
	(defq n (normalize facing) h (:y place) to_eye (normalize (- eye place))
		lit 1.0 colour (vec3 0.0))
	(if (< h (+ water 4.0)) (setq h (ground (:xz place))))
	(if (> shadows 0) (setq lit (sunlit place)))
	(cond
		((<= h water)
			;the water, the sky in it more side on, the sun off it, and the
			;sand showing through where it is shallow
			(defq w (ripple place (- 1.0 fade)) side (pow (- 1.0 (max (dot to_eye w) 0.0)) 3.0)
				glint (pow (max (dot (reflect (- sun) w) to_eye) 0.0) 90.0))
			(setq colour (+ (mix (mix deep (sky_in (reflect (- to_eye) w)) (+ 0.2 (* 0.75 side)))
					(* sand 0.85) (* 0.6 (ease (- water 1.8) water h)))
				(* sun_colour glint lit 2.0))))
		(:t ;the land, of how high and how steep, rough, and lit
			(defq up (:y n) b (bumped place n (* rough (+ 0.3 (* 1.4 (- 1.0 up))))))
			(setq colour (mix sand (mix grass moss (noise3 (* place 0.05))) (ease (+ water 0.5) (+ water 3.5) h))
				colour (mix colour rock (ease 0.86 0.72 up))
				colour (mix colour snow (* (ease 34.0 42.0 h) (ease 0.7 0.85 up)))
				colour (* colour (+ 0.28 (* 0.95 lit (max (dot b sun) 0.0)))))))
	(vec4 (mix colour haze fade) 1.0))
