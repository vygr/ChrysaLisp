;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A craft, a pixel shader. It goes with craft_vertex.shader.
;
; Not of this world, but lit by it. The hull is a metal of its tint
; that mirrors the sky over it and the land under it, Flight's own,
; more side on, with a film on it whose colours run with the angle it
; is seen at, as oil on water, and the sun's spot. What makes it more
; than a shape is drawn on it, not made of triangles: seams between its
; panels, and flecks of lamps round its rim, lit by a wave that runs
; over them. Its
; lights glow of themselves and pulse. All of it goes into the haze as the land does,
; but the lights less: they are seen first, before what they are on.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/flight/land.shader")

;the world: the way to the sun, the haze at the horizon and the blue
;above, where the eye is, and the time, in seconds
(definput sun :vec3)
(definput haze :vec3)
(definput above :vec3)
(definput eye :vec3)
(definput time :float)
;the colour of the light of the sun or the moon, as the day is, on it and
;off it
(definput daylight :vec3)
(definput shine :vec3)
;the part: the colour of its metal, the light it gives, how much of
;the film it has, how many panels across it, 0 for none, and the colour
;of the lamps round its rim, black for none
(definput tint :vec3)
(definput glow :vec3)
(definput film :float)
(definput panels :float)
(definput lamps :vec3)
;a cow's black patches, 1 for them, 0 for none
(definput patches :float)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying fade :float)
(defvarying local :vec3)

;the land as a mirror sees it, far off and in the haze
(defconst far_land (vec3 0.30 0.36 0.26))

;what a way sees: the sky above the horizon, the land below it
(defun seen :vec3 ((d :vec3))
	(defq y (:y d))
	(mix (mix haze far_land (clamp (* y -3.0) 0.0 1.0))
		(mix haze above (pow (max y 0.0) 0.6))
		(clamp (* y 40.0) 0.0 1.0)))

;how near a line of a grid of n across a value is, 1 on it
(defun seam :float ((v :float) (n :float))
	(- 1.0 (ease 0.0 0.06 (abs (- (fract (+ (* v n) 0.5)) 0.5)))))

(defun main :vec4 ((frag :vec2))
	;a cow's patches, of a noise, worked out only for a cow
	(defq blot 0.0)
	(if (> patches 0.0) (setq blot (* patches 0.9 (ease 0.5 0.56 (noise3 (* local 2.6))))))
	(defq n (normalize facing) to_eye (normalize (- eye place))
		r (reflect (- to_eye) n)
		lit (max (dot n sun) 0.0)
		edge (pow (- 1.0 (max (dot n to_eye) 0.0)) 2.0)
		;the film's colours, round the hues as the angle goes
		hue (* 6.283 (+ (* edge 1.5) (* 0.15 (:y n))))
		oil (+ (vec3 0.5) (* 0.5 (vec3 (cos hue) (cos (+ hue 2.094)) (cos (+ hue 4.189)))))
		;the panels, rings round it and lines across, darker at their seams
		round (length (:xz local))
		seams (* (min panels 1.0) (max (seam round panels) (max (seam (:x local) panels) (seam (:z local) panels))))
		metal (* (+ tint (* oil film)) (- 1.0 (* 0.55 seams)) (- 1.0 blot))
		spot (pow (max (dot r sun) 0.0) 80.0)
		pulse (+ 0.7 (* 0.3 (cos (* time 4.0))))
		hide (* tint (- 1.0 blot))
		colour (+ (* (mix (* hide (+ 0.2 (* 0.6 lit))) (* (seen r) metal) (* (+ 0.35 (* 0.6 edge)) (- 1.0 patches))) daylight)
			(* shine spot 1.4)))
	;the lamps, flecks on the rim, each lit as a wave goes over
	(defq spots (pow (max (* (cos (* (:x local) 22.0)) (cos (* (:z local) 22.0))) 0.0) 4.0)
		rim (* (ease 0.62 0.72 round) (- 1.0 (ease 0.04 0.14 (abs (:y local)))))
		wave (pow (+ 0.5 (* 0.5 (cos (- (* time 5.0) (* 3.0 (+ (:x local) (:z local))))))) 3.0)
		light (+ (* glow pulse) (* lamps (* spots rim (+ 0.25 (* 1.5 wave))))))
	(vec4 (+ (mix (* colour (- 1.0 (* 0.4 seams))) haze fade) (* light (- 1.0 (* 0.6 fade)))) 1.0))
