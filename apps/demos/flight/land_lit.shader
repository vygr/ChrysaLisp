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
; shining off them. The craft about, each a ball, cast their shadows on
; it, softly, and their lights light it under them, the nearer the more,
; each its own colour. And far off, all of it goes into the haze.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/flight/land.shader")

;the way to the sun, the colour of the haze and of the sky above, the
;time, how rough the land is, and how many steps to the sun a shadow
;looks, 0 for none: the app gives less of them to the CPU than the GPU
(definput sun :vec3)
;the ways to the sun and the moon themselves, of the glow round each in
;the sky, which the haze far off is too
(definput sun_way :vec3)
(definput moon_way :vec3)
(definput haze :vec3)
(definput above :vec3)
(definput eye :vec3)
(definput time :float)
(definput rough :float 0.0 0.0 1.0)
(definput shadows :int 0 0 24)
;the craft about, each where it is and how big, a radius of 0 for none,
;and the colour of its light
(definput craft_a :vec4)
(definput craft_b :vec4)
(definput craft_c :vec4)
(definput light_a :vec3)
(definput light_b :vec3)
(definput light_c :vec3)
;a craft's beam where it meets the land, across and along, its radius,
;and how far it is on, a radius of 0 for none
(definput beam :vec4)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying fade :float)

(defconst sand (vec3 0.76 0.70 0.50))
(defconst grass (vec3 0.24 0.42 0.16))
(defconst moss (vec3 0.36 0.46 0.20))
(defconst rock (vec3 0.42 0.38 0.34))
(defconst snow (vec3 0.95 0.96 1.0))
(defconst deep (vec3 0.05 0.16 0.24))
;the colour of the light of the sun or the moon, as the day is, on the
;land, and off the water
(definput daylight :vec3)
(definput shine :vec3)
;how much it is night: the moon's glitter on the water wider and brighter
(definput night :float)
;1 for plain, for a GPU that can not keep up: water with no ripples, the
;land's colour with no noise in it
(definput plain :float)
(defconst eps 0.15)
;how far off the land's bump is whole, and how far it is gone
(defconst bump_near 90.0)
(defconst bump_far 320.0)

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

;how much of the sun a craft leaves here: the way to the sun passes the
;ball by so far, inside it none, softly at its edge
(defun craft_shade :float ((p :vec3) (c :vec4))
	(defq v (- (:xyz c) p) t (dot v sun) r (:w c))
	(if (or (<= r 0.0) (<= t 0.0)) (return 1.0))
	(ease (* r 0.45) (* r 1.05) (length (- v (* sun t)))))

;the light a craft gives here, the nearer it the more, on a face turned
;to it, nothing past a few times its size
(defun craft_glow :vec3 ((p :vec3) (n :vec3) (c :vec4) (col :vec3))
	(defq v (- (:xyz c) p) d (length v) r (:w c))
	(if (<= r 0.0) (return (vec3 0.0)))
	(* col (/ (* 1.5 (max (dot n (/ v d)) 0.0)) (+ 1.0 (* (/ d (* r 1.6)) (/ d (* r 1.6)))))))

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

;the sky in a way with the glow round the sun and the moon, as sky_lit.shader
;has it but for their discs and the stars: what the land far off goes into,
;so the edge of it is the sky behind it whichever way it is looked at
(defconst far_sun (vec3 1.0 0.92 0.75))
(defconst far_moon (vec3 0.75 0.82 1.0))
(defun far_sky :vec3 ((d :vec3))
	(defq to_sun (max (dot d sun_way) 0.0) to_moon (max (dot d moon_way) 0.0))
	(min (+ (sky_in d) (* far_sun (* 0.35 (pow to_sun 12.0)))
		(* far_moon night (+ (* 0.12 (pow to_moon 40.0)) (* 0.08 (pow to_moon 6.0))))) (vec3 1.0)))

(defun main :vec4 ((frag :vec2))
	;how high the land is here: near the water the land's own height at this
	;pixel, not the grid's between its corners, so the shore follows the
	;land and not the grid's triangles
	(defq to_eye (normalize (- eye place)))
	;all of it haze: it is the sky that way, and nothing of the land is
	;worked out
	(if (> fade 0.998) (return (vec4 (far_sky (- to_eye)) 1.0)))
	(defq n (normalize facing) h (:y place) lit 1.0 colour (vec3 0.0))
	(if (< h (+ water 4.0)) (setq h (ground (:xz place))))
	(defq crafts (* (craft_shade place craft_a) (craft_shade place craft_b) (craft_shade place craft_c))
		glow (+ (craft_glow place n craft_a light_a) (craft_glow place n craft_b light_b)
			(craft_glow place n craft_c light_c)))
	;the beam's pool, white, as wide as the column's foot, its edge soft
	(if (> (:z beam) 0.0)
		(setq glow (+ glow (* (vec3 1.0 1.0 1.05) (* (:w beam) 1.7
			(- 1.0 (ease (* (:z beam) 0.45) (* (:z beam) 1.35) (length (- (:xz place) (:xy beam))))))))))
	(cond
		((<= h water)
			;the water, the sky in it more side on, the sun off it, and the
			;sand showing through where it is shallow
			(defq w (vec3 0.0 1.0 0.0))
			(if (< plain 0.5) (setq w (ripple place (- 1.0 fade))))
			(defq side (pow (- 1.0 (max (dot to_eye w) 0.0)) 3.0)
				glint (* (pow (max (dot (reflect (- sun) w) to_eye) 0.0) (mix 90.0 22.0 night)) (+ 1.0 (* 2.5 night))))
			;the way to the sun is looked along only where there is a glint
			;for a shadow to take
			(if (and (> shadows 0) (> glint 0.0)) (setq lit (sunlit place)))
			(setq lit (* lit crafts))
			(setq colour (+ (* (mix (mix deep (sky_in (reflect (- to_eye) w)) (+ 0.2 (* 0.75 side)))
					(* sand 0.85) (* 0.6 (ease (- water 1.8) water h))) daylight)
				(* shine glint lit 2.0))))
		(:t ;the land, of how high and how steep, rough, and lit
			;rough by a bump where it is grass or rock, sand and snow smooth,
			;and less of it the further off, none where its grain would be
			;finer than a pixel; not worked out where there is none of it
			(defq up (:y n) b n
				grassy (ease (+ water 0.5) (+ water 3.5) h)
				rocky (ease 0.86 0.72 up)
				snowy (* (ease 34.0 42.0 h) (ease 0.7 0.85 up))
				bump (* rough (+ 0.3 (* 1.4 (- 1.0 up))) (- 1.0 snowy) (- 1.0 (* (- 1.0 grassy) (- 1.0 rocky)))
					(ease bump_far bump_near (length (- eye place)))))
			(if (> bump 0.0) (setq b (bumped place n bump)))
			;the way to the sun is looked along, for a shadow, only by a face
			;turned to it
			(defq light (max (dot b sun) 0.0))
			(if (and (> shadows 0) (> light 0.0)) (setq lit (sunlit place)))
			(setq lit (* lit crafts))
			(defq mottle 0.5)
			(if (< plain 0.5) (setq mottle (noise3 (* place 0.05))))
			(setq colour (mix sand (mix grass moss mottle) grassy)
				colour (mix colour rock rocky)
				colour (mix colour snow snowy)
				colour (* colour daylight (+ 0.28 (* 0.95 lit light))))))
	;the craft's shadows dim the water too, and their lights light both
	(if (<= h water) (setq colour (* colour (+ 0.55 (* 0.45 crafts)))))
	(vec4 (mix (+ colour glow) (far_sky (- to_eye)) fade) 1.0))
