;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; An atom of the Molecule app, a polished ball that fills the frame.
;
; It is the color of its atom, lit, with a white highlight that is
; not that color, and a faint light round its edge, as a thing that
; is polished has. Outside the ball is clear, and its edge is the
; share of the pixel that the ball covers. A pixmap is
; premultiplied, so the color is given times the alpha.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;inputs, name type default min max
(definput resolution :vec2)
;what the atom is the color of, and how much light gets to it, less the
;further off it is, 1 is all of it
(definput color :vec3)
(definput light_level :float 1.0 0.0 1.0)

(defconst ambient 0.3)
(defconst diffuse 0.7)
;how small the highlight is, and how bright
(defconst shine 56.0)
(defconst gleam 0.95)
(defconst rim 0.14)

;the light is up and to the left of the eye, and the highlight is where
;the ball faces half way between the light and the eye
(defglobal light (normalize (vec3 -1.0 -1.0 -2.0)))
(defglobal half_way (normalize (+ light (vec3 0.0 0.0 -1.0))))

(defun main :vec4 ((frag :vec2))
	;the frag coord has y going up, the light is in rows going down
	(defq r (* (:x resolution) 0.5)
		nx (/ (- (:x frag) r) r)
		ny (/ (- r (:y frag)) r)
		d2 (+ (* nx nx) (* ny ny))
		cover (clamp (* (- 1.0 (sqrt d2)) r) 0.0 1.0)
		facing (sqrt (max (- 1.0 d2) 0.0))
		n (vec3 nx ny (- facing))
		edge (- 1.0 facing)
		white (+ (* gleam (pow (max (dot n half_way) 0.0) shine))
			(* rim edge edge edge edge))
		lit (min (+ (* color (+ ambient (* diffuse (max (dot n light) 0.0)))) (vec3 white)) (vec3 1.0)))
	(vec4 (* lit light_level cover) cover))
