;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Surface raymarch, a port of https://vygr.github.io/JS-Raymarch
; The ray for a sample is a function here, pixel_ray, the GLSL has
; it written out seven times, and so the two anti alias branches
; of main, which differ only in their test, are one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;inputs, name type default min max
(definput time :float 0.0)
(definput resolution :vec2)
(definput arg_aa :int 0 0 1)
(definput arg_aa_adaptive :int 0 0 1)
(definput arg_aa_debug :int 0 0 1)
(definput arg_depth :int 1 0 2)
(definput arg_aa_limit :float 16.0 0.0 32.0)
(definput arg_ao :float 0.2 0.0 1.0)
(definput arg_ref :float 0.3 0.0 1.0)
(definput arg_shadow :float 32.0 32.0 256.0)
(definput arg_bump :float 0.0 0.0 0.01)
(definput arg_dis :float 0.0 0.0 0.03)
(definput arg_march :float 0.75 0.25 1.0)

(defconst eps 0.001)
(defconst min_distance 0.005)
(defconst clip_far 8.0)
(defconst fov 0.5)
(defconst bgcolor (vec3 0.0))

(defglobal cam_pos (vec3 0.0 0.0 (* time 0.1)))
(defglobal light_pos (vec3
	(+ (:x cam_pos) (* 0.5 (cos time)))
	(+ (:y cam_pos) (* 0.0 (sin time)))
	(:z cam_pos)))

(defun hash :float ((n :float))
	(return (fract (* (sin n) 43758.5453))))

(defun hash33 :vec3 ((p :vec3))
	(defq n (sin (dot p (vec3 7 157 113))))
	(return (fract (* (vec3 2097152 262144 32768) n))))

(defun noise :float ((x :vec3))
	(defq p (floor x) f (fract x))
	(setq f (* f f (- 3.0 (* 2.0 f))))
	(defq n (+ (* (:x p) 7.0) (* (:y p) 57.0) (* 111.0 (:z p))))
	(return (mix
		(mix (mix (hash (+ n 0.0)) (hash (+ n 7.0)) (:x f))
			(mix (hash (+ n 57.0)) (hash (+ n 64.0)) (:x f)) (:y f))
		(mix (mix (hash (+ n 111.0)) (hash (+ n 118.0)) (:x f))
			(mix (hash (+ n 168.0)) (hash (+ n 175.0)) (:x f)) (:y f))
		(:z f))))

(defun sinusoidal_bump :float ((p :vec3))
	(return (+
		(* (sin (+ (* (:x p) 4.0) (* time 0.97)))
			(cos (+ (* (:y p) 4.0) (* time 2.17)))
			(sin (- (* (:z p) 4.0) (* time 1.31))))
		(* 0.5 (sin (+ (* (:x p) 8.0) (* time 0.57)))
			(cos (+ (* (:y p) 8.0) (* time 2.11)))
			(sin (- (* (:z p) 8.0) (* time 1.23)))))))

(defun bump :float ((p :vec3))
	(return (+ (* (noise (* p 64.0)) 0.667) (* (noise (* p 128.0)) 0.333))))

(defun bumpmap :vec3 ((p :vec3) (n :vec3) (bf :float))
	(defq grad (/ (vec3
		(- (bump (vec3 (+ (:x p) eps) (:y p) (:z p)))
			(bump (vec3 (- (:x p) eps) (:y p) (:z p))))
		(- (bump (vec3 (:x p) (+ (:y p) eps) (:z p)))
			(bump (vec3 (:x p) (- (:y p) eps) (:z p))))
		(- (bump (vec3 (:x p) (:y p) (+ (:z p) eps)))
			(bump (vec3 (:x p) (:y p) (- (:z p) eps))))) (+ eps eps)))
	(setq grad (- grad (* n (dot n grad))))
	(return (normalize (- n (* bf grad)))))

;field equation for a sphere
(defun sphere :float ((p :vec3) (center :vec3) (radius :float))
	(return (- (length (- p center)) radius)))

;field equation for a cube
(defun box :float ((p :vec3) (b :vec3))
	(defq d (- (abs p) b))
	(return (+ (min (max (:x d) (max (:y d) (:z d))) 0.0) (length (max d 0.0)))))

;field equation for a rounded cube
(defun rounded_cube :float ((p :vec3) (xt :vec3) (r :float))
	(return (- (length (max (+ (- (abs p) xt) (vec3 r)) 0.0)) r)))

;smooth min between two values
(defun smin :float ((a :float) (b :float) (k :float))
	(defq h (clamp (+ 0.5 (/ (* 0.5 (- b a)) k)) 0.0 1.0))
	(return (- (mix b a h) (* k h (- 1.0 h)))))

;the scene
(defun scene :float ((p :vec3))
	(defq d 0.0)
	(if (> arg_dis 0.0) (setq d (* (sinusoidal_bump (* p 4.0)) arg_dis)))
	(setq p (- (fract p) 0.5))
	(return (+ (sphere p (vec3 0.0) 0.35) d)))

(defun get_normal :vec3 ((p :vec3))
	(return (normalize (vec3
		(- (scene (vec3 (+ (:x p) eps) (:y p) (:z p)))
			(scene (vec3 (- (:x p) eps) (:y p) (:z p))))
		(- (scene (vec3 (:x p) (+ (:y p) eps) (:z p)))
			(scene (vec3 (:x p) (- (:y p) eps) (:z p))))
		(- (scene (vec3 (:x p) (:y p) (+ (:z p) eps)))
			(scene (vec3 (:x p) (:y p) (- (:z p) eps))))))))

(defun calc_ao :float ((p :vec3) (n :vec3))
	(defq r 0.0 w 1.0)
	(for (i 1 6)
		(defq d0 (* (float i) 0.2))
		(setq r (+ r (* w (- d0 (scene (+ p (* n d0))))))
			w (* w 0.5)))
	(return (- 1.0 (clamp r 0.0 1.0))))

(defun calc_shadow :float ((ro :vec3) (rd :vec3) (l :float) (end :float) (k :float))
	(defq shade 1.0)
	(for (i 0 1000)
		(defq h (scene (+ ro (* rd l))))
		(setq shade (min shade (/ (* k h) l)))
		(if (or (<= shade 0.1) (>= l end)) (break))
		(setq l (+ l h)))
	(return (max shade 0.1)))

(defun lighting :vec3 ((sp :vec3) (sn :vec3) (cp :vec3))
	(defq obj_color (floor (mod sp 2.0))
		ld (- light_pos sp)
		lcolor (vec3 1.0 1.0 1.0)
		len (length ld))
	(setq ld (/ ld len))
	(defq light_atten (min (/ 1.0 (* 0.25 len len)) 1.0)
		ref (reflect (- ld) sn)
		ss (calc_shadow sp ld min_distance len arg_shadow)
		ao (- 1.0 arg_ao))
	(if (< ao 1.0) (setq ao (+ ao (* (- 1.0 ao) (calc_ao sp sn)))))
	(defq ambient 0.05
		specular_power 8.0
		diffuse (max 0.0 (dot sn ld))
		specular (max 0.0 (dot ref (normalize (- cp sp)))))
	(setq specular (pow specular specular_power))
	(return (* (+ (* obj_color (+ (* diffuse 0.8) ambient)) (* specular 0.5))
		lcolor light_atten ss ao)))

(defun ray_march :vec2 ((ro :vec3) (rd :vec3) (l :float) (end :float))
	(defq d 0.0 cnt 0.0)
	(for (i 1 1000)
		(setq d (scene (+ ro (* rd l)))
			l (+ l (* d arg_march))
			cnt (float i))
		(if (or (<= d min_distance) (>= l end)) (break)))
	(when (> d min_distance)
		(setq l end cnt 0.0))
	(return (vec2 l cnt)))

(defun scene_ray :vec4 ((ro :vec3) (rd :vec3))
	(defq p_ray (ray_march ro rd 0.0 clip_far))
	(if (>= (:x p_ray) clip_far) (return (vec4 bgcolor (:y p_ray))))
	(defq sp (+ ro (* rd (:x p_ray)))
		sn (get_normal sp)
		bn sn)
	(if (> arg_bump 0.0) (setq bn (bumpmap sp sn arg_bump)))
	(defq color (lighting sp bn ro)
		ref arg_ref)
	(for (i 1 3)
		(if (> i arg_depth) (break))
		(setq ro sp rd (reflect rd bn))
		(defq r_ray (ray_march ro rd (* min_distance 5.0) clip_far))
		(setq (:y p_ray) (max (:y p_ray) (:y r_ray)))
		(when (>= (:x r_ray) clip_far)
			(setq color (+ color (* bgcolor ref)))
			(break))
		(setq sp (+ ro (* rd (:x r_ray)))
			bn (get_normal sp)
			color (+ color (* (lighting sp bn ro) ref))
			ref (* ref arg_ref)))
	(return (vec4 (clamp color 0.0 1.0) (:y p_ray))))

;the ray through a point on the screen
(defun pixel_ray :vec4 ((cords :vec2) (forward :vec3) (right :vec3) (up :vec3))
	(defq aspect (vec2 (/ (:x resolution) (:y resolution)) 1.0)
		screen_cords (* (- (/ (* 2.0 cords) resolution) 1.0) aspect))
	(return (scene_ray cam_pos (normalize (+ forward
		(* fov (:x screen_cords) right)
		(* fov (:y screen_cords) up))))))

(defun main :vec4 ((frag :vec2))
	(defq lookat (vec3 0.0 0.0 0.0)
		forward (normalize (- lookat cam_pos))
		right (normalize (vec3 (:z forward) 0.0 (- (:x forward))))
		up (normalize (cross forward right))
		color (pixel_ray frag forward right up)
		scale 1.0)
	(when (and (/= arg_aa 0) (or (= arg_aa_adaptive 0) (> (:w color) arg_aa_limit)))
		(setq scale 0.25
			color (+ color
				(pixel_ray (+ frag (vec2 0.5 0.0)) forward right up)
				(pixel_ray (+ frag (vec2 0.0 0.5)) forward right up)
				(pixel_ray (+ frag (vec2 0.5 0.5)) forward right up)))
		(if (and (/= arg_aa_adaptive 0) (/= arg_aa_debug 0))
			(setq color (+ color (vec4 2.0)))))
	(setq color (* color scale)
		(:w color) 1.0)
	(return color))
