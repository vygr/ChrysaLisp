;jit compile apps native functions
(jit "apps/raymarch/" "lisp.vp" '("ray_march" "scene"))

(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defq
	+eps 0.1
	+min_distance 0.01
	+clipfar 8.0
	+march_factor 1.0
	+shadow_softness 64.0
	+attenuation 0.05
	+ambient 0.05
	+ref_coef 0.3
	+ref_depth 2
	+light_pos (fixeds -0.1 -0.1 -3.0))

;field equation for a sphere
; (defun sphere (p c r)
;(- (vec-length (vec-sub p c)) r))

;the scene
(defun scene (p)
	(- (vec-length (nums-sub
		(defq _ (fixeds-frac p))
		(const (fixeds 0.5 0.5 0.5)) _)) 0.35))

(defun ray-march (ray_origin ray_dir l max_l min_distance march_factor)
	(defq i -1 d 1.0)
	(while (and (< (++ i) 1000)
				(> d min_distance)
				(< l max_l))
		(defq d (scene (vec-add ray_origin (vec-scale ray_dir l +fixeds_tmp3) +fixeds_tmp3))
			l (+ l (* d march_factor))))
	(if (> d min_distance) max_l l))

;native versions
(ffi "apps/raymarch/scene" scene)
; (scene nums) -> radius
(ffi "apps/raymarch/ray_march" ray-march)
; (ray-march nums nums num num num num) -> distance

(defun get-normal (p)
	(vec-norm (fixeds
		(- (defq d (scene p)) (scene (vec-add p
			(const (fixeds (neg +eps) 0.0 0.0)) +fixeds_tmp3)))
		(- d (scene (vec-add p
			(const (fixeds 0.0 (neg +eps) 0.0)) +fixeds_tmp3)))
		(- d (scene (vec-add p
			(const (fixeds 0.0 0.0 (neg +eps))) +fixeds_tmp3))))))

(defun shadow (ray_origin ray_dir l max_l k)
	(defq s 1.0 i 1000)
	(while (> (-- i) 0)
		(defq h (scene (vec-add ray_origin
			(vec-scale ray_dir l +fixeds_tmp3) +fixeds_tmp3))
			s (min s (/ (* k h) l)))
		(if (or (<= s 0.1) (>= l max_l))
			(setq i 0)
			(++ l h)))
	(max s 0.1))

(defun lighting (surface_pos surface_norm cam_pos)
	(defq obj_color (vec-floor (vec-mod surface_pos (const (fixeds 2.0 2.0 2.0))))
		light_vec (vec-sub +light_pos surface_pos)
		light_dis (vec-length light_vec)
		light_norm (vec-scale light_vec (/ 1.0 light_dis) light_vec)
		light_atten (min (/ 1.0 (* light_dis light_dis +attenuation)) 1.0)
		ref (vec-reflect (vec-scale light_norm -1.0 +fixeds_tmp3) surface_norm)
		ss (shadow surface_pos light_norm +min_distance light_dis +shadow_softness)
		light_col (vec-scale (const (fixeds 1.0 1.0 1.0)) (* light_atten ss))
		diffuse (max 0.0 (vec-dot surface_norm light_norm))
		specular (max 0.0 (vec-dot ref (vec-norm (vec-sub cam_pos surface_pos +fixeds_tmp3))))
		specular (* specular specular specular specular)
		obj_color (vec-scale obj_color (+ (* diffuse (const (- 1.0 +ambient))) +ambient) +fixeds_tmp3)
		obj_color (vec-add obj_color (fixeds specular specular specular) +fixeds_tmp3))
	(vec-mul obj_color light_col))

(defun scene-ray (ray_origin ray_dir)
	(defq l (ray-march ray_origin ray_dir 0.0 +clipfar +min_distance +march_factor))
	(if (>= l +clipfar)
		(const (cat +fixeds_zero3))
		(progn
			;difuse lighting
			(defq surface_pos (vec-add ray_origin (vec-scale ray_dir l +fixeds_tmp3))
				surface_norm (get-normal surface_pos)
				color (lighting surface_pos surface_norm ray_origin)
				i +ref_depth r +ref_coef)
			;reflections
			(while (and (>= (-- i) 0)
						(< (defq ray_origin surface_pos ray_dir (vec-reflect ray_dir surface_norm)
								l (ray-march ray_origin ray_dir (* +min_distance 10.0) +clipfar +min_distance +march_factor))
							+clipfar))
					(defq surface_pos (vec-add ray_origin (vec-scale ray_dir l +fixeds_tmp3))
						surface_norm (get-normal surface_pos)
						color (vec-add (vec-scale color (- 1.0 r) (const (cat +fixeds_tmp3)))
								(vec-scale (lighting surface_pos surface_norm ray_origin) r +fixeds_tmp3))
						r (* r +ref_coef)))
			(vec-clamp color (const (cat +fixeds_tmp3))
				(const (fixeds 0.999 0.999 0.999))))))

(defun rect (key mbox x y x1 y1 w h)
	(write-int (defq reply (string-stream (cat ""))) (list x y x1 y1))
	(defq w2 (/ w 2) h2 (/ h 2) y (dec y))
	(while (/= (++ y) y1)
		(defq xp (dec x))
		(while (/= (++ xp) x1)
			(defq ray_origin (const (fixeds 0.0 0.0 -3.0)) ray_dir (vec-norm (vec-sub
				(fixeds (/ (* (- xp w2) (const (<< 1 +fp_shift))) w2) (/ (* (- y h2) (const (<< 1 +fp_shift))) h2) 0)
				ray_origin)))
			(bind '(r g b) (scene-ray ray_origin ray_dir))
			(write-int reply (+ +argb_black (>> b 8) (logand g 0xff00) (<< (logand r 0xff00) 8)))
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
