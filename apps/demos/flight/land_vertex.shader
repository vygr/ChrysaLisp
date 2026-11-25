;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The land, a vertex shader, for land_lit.shader.
;
; A vertex is a place on a flat grid, across and along. The grid is
; moved under the eye by whole steps of it, shift, so it is always
; round where we are, and the ground is raised there to how high the
; land is at that place of the world, land.shader: so the land rolls
; by under the grid, and nothing is made again. Under the water it is
; the water's flat face. The pixel shader is handed where the place is
; in the world, which way the land faces there, and how far into the
; haze it is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/flight/land.shader")

;how the world is seen, the grid's place, and the eye
(definput lens :mat4)
(definput shift :vec2)
(definput eye :vec3)
;1 for a slope of three samples of the height, not five, for a GPU that
;can not keep up
(definput quick :float)

(defattr position :vec2)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying fade :float)

;how far the slope is measured, and where the haze begins and is whole
(defconst slope 1.5)
(defconst haze_near 200.0)
(defconst haze_far 560.0)
;past where the haze is whole by more than a cell of the grid is across,
;every triangle a vertex is of is all haze, and its slope is not looked at
(defconst haze_all 580.0)

(defun main :vec4 ()
	(defq p (+ position shift) h (ground p) dx 0.0 dz 0.0)
	(setq facing (vec3 0.0 1.0 0.0))
	(if (< h water) (setq h water))
	(setq place (vec3 (:x p) h (:y p)))
	(defq far (length (- place eye)))
	(setq fade (ease haze_near haze_far far))
	;the slope, of the land that is over the water and not all haze
	(when (and (> h water) (< far haze_all))
		(if (> quick 0.5)
			;one way from here, each twice as far, the height here had already
			(setq dx (* 2.0 (- (ground (+ p (vec2 slope 0.0))) h))
				dz (* 2.0 (- (ground (+ p (vec2 0.0 slope))) h)))
			(setq dx (- (ground (+ p (vec2 slope 0.0))) (ground (- p (vec2 slope 0.0))))
				dz (- (ground (+ p (vec2 0.0 slope))) (ground (- p (vec2 0.0 slope))))))
		(setq facing (normalize (vec3 (- dx) (* 2.0 slope) (- dz)))))
	(* lens (vec4 place 1.0)))
