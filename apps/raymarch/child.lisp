;jit compile apps native functions
(jit "apps/raymarch/" "lisp.vp" '("ray_march" "scene"))

(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defq
	+ref_depth 2
	+real_1000 (n2r 1000)
	+real_255 (n2r 255)
	+eps (n2r 0.1)
	+min_distance (n2r 0.01)
	+clipfar +real_8
	+march_factor +real_1
	+shadow_softness (n2r 64.0)
	+attenuation (n2r 0.05)
	+ambient (n2r 0.05)
	+ref_coef (n2r 0.25)
	+light_pos (reals (n2r -0.1) (n2r -0.1) (n2r -3.0)))

;field equation for a sphere
; (defun sphere (p c r)
;(- (vec-length (vec-sub p c)) r))

;the scene
(defun scene (p)
	(- (vec-length (nums-sub
		(defq _ (fixeds-frac p))
		(const (reals +real_1/2 +real_1/2 +real_1/2)) _)) (const (n2r 0.35))))

(defun ray-march (ray_origin ray_dir l max_l min_distance march_factor)
	(defq i -1 d +real_1)
	(while (and (< (++ i) 1000)
				(> d min_distance)
				(< l max_l))
		(defq d (scene (vec-add ray_origin (vec-scale ray_dir l +reals_tmp3) +reals_tmp3))
			l (+ l (* d march_factor))))
	(if (> d min_distance) max_l l))

;native versions
(ffi "apps/raymarch/scene" scene)
; (scene reals) -> radius
;(ffi "apps/raymarch/ray_march" ray-march)
; (ray-march reals reals real real real real) -> distance

(defun get-normal (p)
	(vec-norm (reals
		(- (defq d (scene p)) (scene (vec-add p
			(const (reals (neg +eps) +real_0 +real_0)) +reals_tmp3)))
		(- d (scene (vec-add p
			(const (reals +real_0 (neg +eps) +real_0)) +reals_tmp3)))
		(- d (scene (vec-add p
			(const (reals +real_0 +real_0 (neg +eps))) +reals_tmp3))))))

(defun shadow (ray_origin ray_dir l max_l k)
	(defq s +real_1 i 1000)
	(while (> (-- i) 0)
		(defq h (scene (vec-add ray_origin
			(vec-scale ray_dir l +reals_tmp3) +reals_tmp3))
			s (min s (/ (* k h) l)))
		(if (or (<= s +real_1/10) (>= l max_l))
			(setq i 0)
			(++ l h)))
	(max s +real_1/10))

(defun lighting (surface_pos surface_norm cam_pos)
	(defq obj_color (vec-floor (vec-mod (vec-add surface_pos
			(const (reals +real_1000 +real_1000 +real_1000)))
			(const (reals +real_2 +real_2 +real_2))))
		light_vec (vec-sub +light_pos surface_pos)
		light_dis (vec-length light_vec)
		light_norm (vec-scale light_vec (/ +real_1 light_dis) light_vec)
		light_atten (min (/ +real_1 (* light_dis light_dis +attenuation)) +real_1)
		ref (vec-reflect (vec-scale light_norm +real_-1 +reals_tmp3) surface_norm)
		ss (shadow surface_pos light_norm +min_distance light_dis +shadow_softness)
		light_col (vec-scale (const (reals +real_1 +real_1 +real_1)) (* light_atten ss))
		diffuse (max +real_0 (vec-dot surface_norm light_norm))
		specular (max +real_0 (vec-dot ref (vec-norm (vec-sub cam_pos surface_pos +reals_tmp3))))
		specular (* specular specular specular specular)
		obj_color (vec-scale obj_color (+ (* diffuse (const (- +real_1 +ambient))) +ambient) +reals_tmp3)
		obj_color (vec-add obj_color (reals specular specular specular) +reals_tmp3))
	(vec-mul obj_color light_col))

(defun scene-ray (ray_origin ray_dir)
	(defq l (ray-march ray_origin ray_dir +real_0 +clipfar +min_distance +march_factor))
	(if (>= l +clipfar)
		(const (cat +reals_zero3))
		(progn
			;diffuse lighting
			(defq surface_pos (vec-add ray_origin (vec-scale ray_dir l +reals_tmp3))
				surface_norm (get-normal surface_pos)
				color (lighting surface_pos surface_norm ray_origin)
				i +ref_depth r +ref_coef)
			;reflections
			(while (and (>= (-- i) 0)
						(< (defq ray_origin surface_pos ray_dir (vec-reflect ray_dir surface_norm)
								l (ray-march ray_origin ray_dir (* +min_distance +real_10) +clipfar +min_distance +march_factor))
							+clipfar))
					(defq surface_pos (vec-add ray_origin (vec-scale ray_dir l +reals_tmp3))
						surface_norm (get-normal surface_pos)
						color (vec-add (vec-scale color (- +real_1 r) (const (cat +reals_tmp3)))
								(vec-scale (lighting surface_pos surface_norm ray_origin) r +reals_tmp3))
						r (* r +ref_coef)))
			(vec-clamp color (const (cat +reals_tmp3)) +reals_one3))))

(defun rect (key mbox x y x1 y1 w h)
	(write-int (defq reply (string-stream (cat ""))) (list x y x1 y1))
	(bind '(x y x1 y1 w h) (map (const n2r) (list x y x1 y1 w h)))
	(defq w2 (/ w +real_2) h2 (/ h +real_2) y (- y +real_1))
	(while (< (setq y (+ y +real_1)) y1)
		(defq xp (- x +real_1))
		(while (< (setq xp (+ xp +real_1)) x1)
			(defq ray_origin (const (reals +real_0 +real_0 +real_-3))
				ray_dir (vec-norm (vec-sub
					(reals (/ (* (- xp w2) +real_1) w2)
						(/ (* (- y h2) +real_1) h2) +real_0) ray_origin)))
			(bind '(r g b) (scene-ray ray_origin ray_dir))
			(write-int reply (+ +argb_black
				(<< (n2i (* r +real_255)) 16)
				(<< (n2i (* g +real_255)) 8)
				(n2i (* b +real_255))))
			(task-slice)))
	(write-long reply key)
	(mail-send mbox (str reply)))

(defun main ()
	(defq select (task-mboxes +select_size) running :t +timeout 5000000)
	(while running
		(mail-timeout (elem-get select +select_timeout) +timeout 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running :nil))
			((= idx +select_main)
				;main mailbox, reset timeout and reply with result
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(defq key (getf msg +job_key)
					mbox (getf msg +job_reply)
					msg (slice msg +job_x -1))
				(apply rect (cat (list key mbox) (map (lambda (_) (get-long msg (* _ +long_size))) (range 0 6))))))))
