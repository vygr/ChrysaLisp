;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Raymarch film, a camera that flies into a lattice of balls,
; with a light that swings beside it. Soft shadows, a highlight,
; and two bounces of reflection.
;
; It was Lisp, with two functions of VP by hand, on the nodes. As
; a shader the GPU draws it, and the nodes still can, as native
; code, if there is none.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;inputs, name type default min max
(definput resolution :vec2)
(definput cam_z :float -3.0 -3.0 -1.0)
(definput light_x :float -0.1 -0.25 0.05)

(defconst ref_depth 2)
(defconst eps 0.1)
(defconst min_distance 0.01)
(defconst clip_far 8.0)
(defconst shadow_softness 64.0)
(defconst attenuation 0.05)
(defconst ambient 0.05)
(defconst ref_coef 0.25)

(defglobal cam_pos (vec3 0.0 0.0 cam_z))
(defglobal light_pos (vec3 light_x -0.1 cam_z))

(defun scene :float ((p :vec3))
	;a ball in every unit cube
	(- (length (- (fract p) (vec3 0.5))) 0.35))

(defun march :float ((origin :vec3) (dir :vec3) (start :float))
	;how far along the ray the scene is, clip_far if it is not met
	(defq l start d 1.0)
	(for (i 0 1000)
		(when (or (<= d min_distance) (>= l clip_far)) (break))
		(setq d (scene (+ origin (* dir l))))
		(setq l (+ l d)))
	(if (> d min_distance) clip_far l))

(defun normal :vec3 ((p :vec3))
	(defq d (scene p))
	(normalize (vec3
		(- d (scene (+ p (vec3 (- eps) 0.0 0.0))))
		(- d (scene (+ p (vec3 0.0 (- eps) 0.0))))
		(- d (scene (+ p (vec3 0.0 0.0 (- eps))))))))

(defun shadow :float ((origin :vec3) (dir :vec3) (start :float) (max_l :float))
	;how much of the light gets to a point, soft at the edge of a shadow
	(defq s 1.0 l start)
	(for (i 0 999)
		(defq h (scene (+ origin (* dir l))))
		(setq s (min s (/ (* shadow_softness h) l)))
		(when (or (<= s 0.1) (>= l max_l)) (break))
		(setq l (+ l h)))
	(max s 0.1))

(defun lighting :vec3 ((pos :vec3) (norm :vec3) (eye :vec3))
	;the colour of a point of a ball, as seen from the eye. The balls are
	;coloured by which cube they are in
	(defq obj_color (floor (mod (+ pos (vec3 1000.0)) 2.0))
		light_vec (- light_pos pos)
		light_dis (length light_vec)
		light_norm (/ light_vec light_dis)
		light_atten (min (/ 1.0 (* light_dis light_dis attenuation)) 1.0)
		ref (reflect (- light_norm) norm)
		lit (shadow pos light_norm min_distance light_dis)
		diffuse (max 0.0 (dot norm light_norm))
		spec (max 0.0 (dot ref (normalize (- eye pos))))
		spec4 (* spec spec spec spec))
	(* (+ (* obj_color (+ (* diffuse (- 1.0 ambient)) ambient)) (vec3 spec4))
		(* light_atten lit)))

(defun scene_ray :vec3 ((origin :vec3) (dir :vec3))
	;the colour a ray sees, the ball it meets and what that reflects
	(defq l (march origin dir 0.0))
	(when (>= l clip_far) (return (vec3 0.0)))
	(defq pos (+ origin (* dir l))
		norm (normal pos)
		color (lighting pos norm origin)
		ro origin rd dir r ref_coef)
	(for (i 0 ref_depth)
		(setq ro pos)
		(setq rd (reflect rd norm))
		(defq hit (march ro rd (* min_distance 10.0)))
		(when (>= hit clip_far) (break))
		(setq pos (+ ro (* rd hit)))
		(setq norm (normal pos))
		(setq color (+ (* color (- 1.0 r)) (* (lighting pos norm ro) r)))
		(setq r (* r ref_coef)))
	(min (max color 0.0) 1.0))

(defun main :vec4 ((frag :vec2))
	;the frag coord has y going up, the film has row 0 at the top, and a
	;ray goes through the corner of its pixel, as the Lisp had it
	(defq half (* resolution 0.5)
		px (- (:x frag) 0.5)
		row (- (:y resolution) 0.5 (:y frag))
		dir (normalize (vec3
			(/ (- px (:x half)) (:x half))
			(/ (- row (:y half)) (:y half))
			3.0)))
	(vec4 (scene_ray cam_pos dir) 1.0))
