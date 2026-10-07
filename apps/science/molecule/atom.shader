;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; An atom of the Molecule app, a lit ball that fills the frame.
;
; It is grey, the app draws it in the color of the atom. Outside
; the ball is clear, and its edge is the share of the pixel that
; the ball covers. A pixmap is premultiplied, so the grey is given
; times the alpha.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;inputs, name type default min max
(definput resolution :vec2)

(defconst ambient 0.3)
(defconst diffuse 0.7)
(defconst shine 256.0)

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
		n (vec3 nx ny (- (sqrt (max (- 1.0 d2) 0.0))))
		grey (min 1.0 (+ ambient
			(* diffuse (max (dot n light) 0.0))
			(pow (max (dot n half_way) 0.0) shine))))
	(vec4 (* (vec3 grey) cover) cover))
