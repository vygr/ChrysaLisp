;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Where the atoms of the Molecule app are, a vertex shader.
;
; It is used on its own, with no pixel shader. The app has it place
; every atom, as native code, and draws a picture of a ball at each
; from what comes back. Its varyings are what the app wants to know
; of an atom, where on the widget it is, and how big, and how far
; away.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;how the molecule is turned, moved back, and seen
(definput spin :mat4)
(definput move :mat4)
(definput lens :mat4)
;the middle of the widget, and half the size of the view on it
(definput centre :vec2)
(definput half :float)

;an atom, where it is in the molecule, and its radius
(defattr atom :vec4)
(defattr radius :float)

;on the widget, x and y, and its radius in pixels
(defvarying spot :vec3)
;how deep it is, -1 to 1 is in view, and how much light gets to it
(defvarying depth :vec2)

;the three as one, worked out once for all the atoms
(defglobal matrix (* lens move spin))

(defun main :vec4 ()
	(defq p (* matrix atom)
		rw (/ 1.0 (:w p))
		z (* (:z p) rw))
	(setq spot (vec3
			(+ (:x centre) (* (:x p) rw half))
			(+ (:y centre) (* (:y p) rw half))
			(* radius half rw))
		depth (vec2 z (/ 1.0 (+ z 2.0))))
	p)
