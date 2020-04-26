;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

(structure 'work 0
	(long 'width 'height 'y))

(defq
	eps 0.1
	min_distance 0.01
	clipfar 8.0
	march_factor 1.0
	shadow_softness 64.0
	attenuation 0.05
	ambient 0.05
	ref_coef 0.25
	ref_depth 1
	light_pos (nums -0.1 -0.1 -3.0))

;field equation for a sphere
;(defun sphere (p c r)
;	(- (vec-length (vec-sub p c)) r))

;the scene
(defun-bind scene (p)
	(- (vec-length
		(nums-sub
			(defq _ (nums-frac p))
			(const (nums 0.5 0.5 0.5)) _)) 0.35))

(defun-bind ray-march (ray_origin ray_dir l max_l min_distance march_factor)
	(defq i -1 d 1.0 _ (nums 0 0 0))
	(while (and (< (setq i (inc i)) 1000)
				(> d min_distance)
				(< l max_l))
		(defq d (scene (vec-add ray_origin (vec-scale ray_dir l _) _))
			l (+ l (* d march_factor))))
	(if (> d min_distance) max_l l))

;native versions
(ffi scene "apps/raymarch/scene" 0)
(ffi ray-march "apps/raymarch/ray-march" 0)

(defun-bind get-normal (p)
	(bind '(x y z) p)
	(defq d (scene p) x (fixed x) y (fixed y) z (fixed z))
	(vec-norm (nums
		(- d (scene (nums (- x (const eps)) y z)))
		(- d (scene (nums x (- y (const eps)) z)))
		(- d (scene (nums x y (- z (const eps))))))))

(defun-bind shadow (ray_origin ray_dir l max_l k)
	(defq s 1.0 i 1000 _ (nums 0.0 0.0 0.0))
	(while (> (setq i (dec i)) 0)
		(defq h (scene (vec-add ray_origin (vec-scale ray_dir l _) _))
			s (min s (/ (* k h) l)))
		(if (or (<= s 0.1) (>= l max_l))
			(setq i 0)
			(setq l (+ l h))))
	(max s 0.1))

(defun-bind lighting (surface_pos surface_norm cam_pos)
	(defq _ (nums 0 0 0) obj_color (vec-floor (vec-mod surface_pos (<< 2 fp_shift)))
		light_vec (vec-sub light_pos surface_pos)
		light_dis (fixed (vec-length light_vec))
		light_norm (vec-scale light_vec (/ 1.0 light_dis) light_vec)
		light_atten (min (/ 1.0 (* light_dis light_dis attenuation)) 1.0)
		ref (vec-reflect (vec-scale light_norm -1.0 _) surface_norm)
		ss (shadow surface_pos light_norm min_distance light_dis shadow_softness)
		light_col (vec-scale (const (nums 1.0 1.0 1.0)) (* light_atten ss))
		diffuse (max 0.0 (fixed (vec-dot surface_norm light_norm _)))
		specular (max 0.0 (fixed (vec-dot ref (vec-norm (vec-sub cam_pos surface_pos) _) _)))
		specular (* specular specular specular specular)
		obj_color (vec-scale obj_color (+ (* diffuse (- 1.0 ambient)) ambient) _)
		obj_color (vec-add obj_color (nums specular specular specular) _))
	(vec-mul obj_color light_col _))

(defun-bind scene-ray (ray_origin ray_dir)
	(defq l (ray-march ray_origin ray_dir 0.0 clipfar min_distance march_factor))
	(if (>= l clipfar)
		(const (nums 0.0 0.0 0.0))
		(progn
			(defq surface_pos (vec-add ray_origin (vec-scale ray_dir l))
				surface_norm (get-normal surface_pos)
				color (lighting surface_pos surface_norm ray_origin)
				i ref_depth r ref_coef)
			(while (and (>= (setq i (dec i)) 0)
						(< (defq ray_origin surface_pos ray_dir (vec-reflect ray_dir surface_norm)
								l (ray-march ray_origin ray_dir (* min_distance 2.0) clipfar min_distance march_factor))
							clipfar))
					(defq surface_pos (vec-add ray_origin (vec-scale ray_dir l))
						surface_norm (get-normal surface_pos)
						color (vec-add (vec-scale color (- 1.0 r))
								(vec-scale (lighting surface_pos surface_norm ray_origin) r))
						r (* r ref_coef)))
			(vec-clamp color 0 (logior 0.999)))))

(defun-bind rect (mbox x y x1 y1 w h)
	(write-int (defq reply (string-stream (cat ""))) (list x y x1 y1))
	(defq w2 (/ w 2) h2 (/ h 2) y (dec y))
	(while (/= (setq y (inc y)) y1)
		(defq xp (dec x))
		(while (/= (setq xp (inc xp)) x1)
			(defq
				ray_origin (const (nums 0 0 -3.0))
				ray_dir (vec-norm (vec-sub
					(nums (/ (* (- xp w2) (const (<< 1 fp_shift))) w2) (/ (* (- y h2) (const (<< 1 fp_shift))) h2) 0.0)
					ray_origin)))
			(bind '(r g b) (scene-ray ray_origin ray_dir))
			(write-int reply (+ argb_black (>> b 8) (logand g 0xff00) (<< (logand r 0xff00) 8)))
		(task-sleep 0)))
	(write-long reply (task-mailbox))
	(mail-send (str reply) mbox))

(defun-bind main ()
	;read work request or exit
	(while (/= 0 (length (defq msg (mail-read (task-mailbox)))))
		(setq msg (string-stream msg))
		(apply rect (map (lambda (_) (read-long msg)) (range 0 7)))))
