;jit compile apps native functions
(defq *app_root* (path-to-file))
(jit *app_root* "lisp.vp" '("ray_march" "scene"))

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
	+ref_coef (n2r 0.25))

;field equation for a sphere
; (defun sphere (p c r)
;(- (vector-length (vector-sub p c)) r))

;the scene
(defun scene (p)
	(- (vector-length (nums-sub
		(defq _ (fixeds-frac p))
		(const (reals +real_1/2 +real_1/2 +real_1/2)) _)) (const (n2r 0.35))))

(defun ray-march (ray_origin ray_dir l max_l min_distance march_factor)
	(defq i -1 d +real_1)
	(while (and (< (++ i) 1000) (> d min_distance) (< l max_l))
		(defq d (scene (vector-add ray_origin (vector-scale ray_dir l +reals_tmp3) +reals_tmp3))
			l (+ l (* d march_factor))))
	(if (> d min_distance) max_l l))

;native versions
(ffi (cat *app_root* "scene") scene)
; (scene reals) -> radius
(ffi (cat *app_root* "ray_march") ray-march)
; (ray-march reals reals real real real real) -> distance

(defun get-normal (p)
	(vector-norm (reals
		(- (defq d (scene p)) (scene (vector-add p
			(const (reals (neg +eps) +real_0 +real_0)) +reals_tmp3)))
		(- d (scene (vector-add p
			(const (reals +real_0 (neg +eps) +real_0)) +reals_tmp3)))
		(- d (scene (vector-add p
			(const (reals +real_0 +real_0 (neg +eps))) +reals_tmp3))))))

(defun shadow (ray_origin ray_dir l max_l k)
	(defq s +real_1 i 1000)
	(while (> (-- i) 0)
		(defq h (scene (vector-add ray_origin
				(vector-scale ray_dir l +reals_tmp3) +reals_tmp3))
			s (min s (/ (* k h) l)))
		(if (or (<= s +real_1/10) (>= l max_l))
			(setq i 0)
			(++ l h)))
	(max s +real_1/10))

(defun lighting (surface_pos surface_norm cam_pos light_pos)
	(defq obj_color (vector-floor (vector-mod (vector-add surface_pos
			(const (reals +real_1000 +real_1000 +real_1000)))
			(const (reals +real_2 +real_2 +real_2))))
		light_vec (vector-sub light_pos surface_pos)
		light_dis (vector-length light_vec)
		light_norm (vector-scale light_vec (/ +real_1 light_dis) light_vec)
		light_atten (min (/ +real_1 (* light_dis light_dis +attenuation)) +real_1)
		ref (vector-reflect (vector-scale light_norm +real_-1 +reals_tmp3) surface_norm)
		ss (shadow surface_pos light_norm +min_distance light_dis +shadow_softness)
		light_col (vector-scale (const (reals +real_1 +real_1 +real_1)) (* light_atten ss))
		diffuse (max +real_0 (vector-dot surface_norm light_norm))
		specular (max +real_0 (vector-dot ref (vector-norm (vector-sub cam_pos surface_pos +reals_tmp3))))
		specular (* specular specular specular specular)
		obj_color (vector-scale obj_color (+ (* diffuse (const (- +real_1 +ambient))) +ambient) +reals_tmp3)
		obj_color (vector-add obj_color (reals specular specular specular) +reals_tmp3))
	(vector-mul obj_color light_col))

(defun scene-ray (ray_origin ray_dir light_pos)
	(defq l (ray-march ray_origin ray_dir +real_0 +clipfar +min_distance +march_factor))
	(if (>= l +clipfar)
		(const (cat +reals_zero3))
		(progn
			;diffuse lighting
			(defq surface_pos (vector-add ray_origin (vector-scale ray_dir l +reals_tmp3))
				surface_norm (get-normal surface_pos)
				color (lighting surface_pos surface_norm ray_origin light_pos)
				i +ref_depth r +ref_coef)
			;reflections
			(while (and (>= (-- i) 0)
						(< (defq ray_origin surface_pos ray_dir (vector-reflect ray_dir surface_norm)
								l (ray-march ray_origin ray_dir (* +min_distance +real_10) +clipfar +min_distance +march_factor))
							+clipfar))
					(defq surface_pos (vector-add ray_origin (vector-scale ray_dir l +reals_tmp3))
						surface_norm (get-normal surface_pos)
						color (vector-add (vector-scale color (- +real_1 r) (const (cat +reals_tmp3)))
								(vector-scale (lighting surface_pos surface_norm ray_origin light_pos) r +reals_tmp3))
						r (* r +ref_coef)))
			(vector-clamp color (const (cat +reals_tmp3)) +reals_one3))))

;the canvas of the app, on its pixels in shared memory, and the key they
;were found by
(defq shared_key 0 canvas :nil)

(defun attach (key w h)
	;the app's canvas, found again if the key has changed. :nil if this
	;node can not reach the pixels, it is on another machine say
	(unless (= key shared_key)
		(setq shared_key key canvas (and (/= key 0) (canvas-shared w h 1 key))))
	canvas)

(defun rect (key mbox x y x1 y1 canvas_key w h cam_z light_x light_z)
	;march the tile. It is drawn straight onto the app's canvas if that
	;can be reached, and only the word that it is done goes back. If not
	;the pixels go back.
	(defq pixels (string-stream (str-alloc (* (- x1 x) (- y1 y) +int_size)))
		ty y w2 (/ w +real_2) h2 (/ h +real_2) y (dec y)
		light_pos (reals light_x (n2r -0.1) light_z)
		screen_z (+ cam_z (const (n2r 3.0))))
	(while (< (++ y) y1)
		(defq xp (dec x))
		(while (< (++ xp) x1)
			(defq ray_origin (reals +real_0 +real_0 cam_z)
				ray_dir (vector-norm (vector-sub
					(reals (/ (* (- (n2r xp) w2) +real_1) w2)
						(/ (* (- (n2r y) h2) +real_1) h2) screen_z) ray_origin)))
			(write-int pixels (reduce! (# (+ %0 (<< (n2i %1) %2))) (list
				(vector-scale (scene-ray ray_origin ray_dir light_pos) +real_255 +reals_tmp3)
				'(16 8 0)) +argb_black))
			(task-slice)))
	(defq pixels (str pixels)
		reply (setf-> (str-alloc +tile_reply_size)
			(+job_reply_key key)
			(+tile_reply_x x) (+tile_reply_y ty)
			(+tile_reply_x1 x1) (+tile_reply_y1 y1)))
	(cond
		((attach canvas_key (n2i w) (n2i h))
			(. canvas :tile pixels x ty x1 y1)
			(mail-send mbox reply))
		((mail-send mbox (cat reply pixels)))))

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
				(apply rect (getf-> msg +job_key +job_reply
					+tile_x +tile_y +tile_x1 +tile_y1 +tile_shared
					+tile_w +tile_h +tile_cam_z +tile_light_x +tile_light_z))))))
