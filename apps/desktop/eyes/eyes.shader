;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The two eyes of the Eyes app, side by side, each a ball that
; fills the height of the frame.
;
; An eye is a ball that is turned to look, so its iris and pupil
; are on the ball, and go round to the side of it, and thin, as it
; looks away. The white is lit, and dim at its rim. The iris has
; fibres that run out from the pupil and a dark ring round it, and
; the whole eye is wet, a highlight sits where the light is and
; does not turn with it. Outside an eye is clear, and its edge is
; the share of the pixel that the ball covers. A pixmap is
; premultiplied, so the color is given times the alpha.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;inputs, name type default min max
(definput resolution :vec2)
;the way each eye looks, a unit vector, x to the right, y down, and z,
;which is less than 0, out of the screen
(definput look_left :vec3)
(definput look_right :vec3)
(definput iris_color :vec3)
;how big the iris is, of the ball, and the pupil, of the iris
(definput iris_size :float 0.5 0.2 0.8)
(definput pupil_size :float 0.4 0.1 0.9)

(defconst ambient 0.5)
(defconst diffuse 0.55)
;how small the highlight is, and how bright
(defconst shine 90.0)
(defconst gleam 0.95)
;how many fibres the iris has, near enough
(defconst fibres 21.0)

;the light is up and to the left of the one who looks at the eyes, and
;the highlight is where the ball faces half way between the two
(defglobal light (normalize (vec3 -1.0 -1.0 -2.0)))
(defglobal half_way (normalize (+ light (vec3 0.0 0.0 -1.0))))

(defun eye :vec4 ((p :vec2) (gaze :vec3) (r :float))
	;p is where on the eye, its middle 0 0 and its edge 1 from that, r how
	;many pixels that 1 is
	(defq d2 (dot p p)
		cover (clamp (* (- 1.0 (sqrt d2)) r) 0.0 1.0)
		facing (sqrt (max (- 1.0 d2) 0.0))
		n (vec3 (:x p) (:y p) (- facing))
		;how far round the ball from where it looks, as a sine, the pupil
		;and the iris are all that is within so far
		along (dot n gaze)
		across (- n (* gaze along))
		off (+ (length across) (* (max (- along) 0.0) 4.0))
		in_iris (clamp (* (- iris_size off) r) 0.0 1.0)
		in_pupil (clamp (* (- (* iris_size pupil_size) off) r 0.5) 0.0 1.0)
		;the white, a little pink out at the rim, and shaded as a ball is
		rim (* d2 d2)
		white (mix (vec3 1.0 1.0 1.0) (vec3 0.96 0.8 0.78) rim)
		shade (* (+ ambient (* diffuse (max (dot n light) 0.0))) (- 1.0 (* 0.3 rim d2)))
		;the iris. Which way round it a point is, is two numbers that go
		;round with it, and the fibres are waves of them
		side (normalize (cross gaze (vec3 0.0 1.0 0.0)))
		up (cross gaze side)
		round_it (/ across (max (length across) 0.0001))
		fibre (+ 0.5 (* 0.25 (sin (* fibres (dot round_it side))))
			(* 0.25 (sin (* fibres 1.31 (dot round_it up)))))
		out (clamp (/ off iris_size) 0.0 1.0)
		ring (- 1.0 (* 0.75 (clamp (* (- out 0.82) 6.0) 0.0 1.0)))
		iris (* iris_color (+ 0.5 (* 0.6 fibre)) (mix 1.25 0.75 (* out out)) ring)
		;the iris is a dish, lit from the side the light is not
		dish (+ 0.75 (* 0.35 (max (dot round_it (- light)) 0.0) out))
		color (mix (* white shade) (* iris dish shade) in_iris)
		color2 (mix color (vec3 0.02 0.02 0.03) in_pupil)
		;wet, a highlight, and a small one across from it
		wet (+ (* gleam (pow (max (dot n half_way) 0.0) shine))
			(* 0.25 (pow (max (dot n (normalize (vec3 0.5 0.6 -1.0))) 0.0) 200.0)))
		lit (min (+ color2 (vec3 wet)) (vec3 1.0)))
	(vec4 (* lit cover) cover))

(defun main :vec4 ((frag :vec2))
	;the frag coord has y going up, an eye is in rows going down
	(defq r (* (:y resolution) 0.48)
		quarter (* (:x resolution) 0.25)
		y (- (* (:y resolution) 0.5) (:y frag)))
	(if (< (:x frag) (* quarter 2.0))
		(eye (/ (vec2 (- (:x frag) quarter) y) r) look_left r)
		(eye (/ (vec2 (- (:x frag) (* quarter 3.0)) y) r) look_right r)))
