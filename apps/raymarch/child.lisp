;imports
(import 'sys/lisp.inc)
(import 'apps/math.inc)

(structure 'work 0
	(long 'parent_id)
	(long 'width)
	(long 'height)
	(long 'y))

(structure 'reply 0
	(long 'child_id)
	(int 'y)
	(offset 'data))

(defq
	eps 0.02
	min_distance 0.01
	clipfar 8.0
	march_factor 1.0
	shadow_softness 64.0
	attenuation 0.05
	ambient 0.05
	ref_coef 0.25
	ref_depth 1
	light_pos (points -0.1 -0.1 -3.0))

;field equation for a sphere
;(defun sphere (p c r)
;	(sub (vec-length (vec-sub p c)) r))

;the scene
(defun-bind scene (p)
	(sub (vec-length
		(points-sub
			(defq _ (points-frac p))
			(const (points 0.5 0.5 0.5)) _)) 0.35))

(defun-bind ray-march (ray_origin ray_dir l max_l min_distance march_factor)
	(defq i -1 d 1.0 _ (points 0 0 0))
	(while (and (lt (setq i (inc i)) 1000)
				(gt d min_distance)
				(lt l max_l))
		(defq d (scene (vec-add ray_origin (vec-scale ray_dir l _) _))
			l (add l (fmul d march_factor))))
	(if (gt d min_distance) max_l l))

;native versions
(ffi scene "apps/raymarch/scene" 0)
(ffi ray-march "apps/raymarch/ray-march" 0)

(defun-bind get-normal ((x y z))
	(vec-norm (points
		(sub (scene (points (add x eps) y z)) (scene (points (sub x eps) y z)))
		(sub (scene (points x (add y eps) z)) (scene (points x (sub y eps) z)))
		(sub (scene (points x y (add z eps))) (scene (points x y (sub z eps)))))))

(defun-bind shadow (ray_origin ray_dir l max_l k)
	(defq s 1.0 i 1000 _ (points 0 0 0))
	(while (gt (setq i (dec i)) 0)
		(defq h (scene (vec-add ray_origin (vec-scale ray_dir l _) _))
			s (min s (fdiv (fmul k h) l)))
		(if (or (le s 0.1) (ge l max_l))
			(setq i 0)
			(setq l (add l h))))
	(max s 0.1))

(defun-bind lighting (surface_pos surface_norm cam_pos)
	(defq _ (points 0 0 0) obj_color (vec-floor (vec-mod surface_pos 2.0))
		light_vec (vec-sub light_pos surface_pos)
		light_dis (vec-length light_vec)
		light_norm (vec-scale light_vec (fdiv 1.0 light_dis) light_vec)
		light_atten (min (fdiv 1.0 (fmul light_dis light_dis attenuation)) 1.0)
		ref (vec-reflect (vec-scale light_norm -1.0 _) surface_norm)
		ss (shadow surface_pos light_norm min_distance light_dis shadow_softness)
		light_col (vec-scale (const (points 1.0 1.0 1.0)) (fmul light_atten ss))
		diffuse (max 0.0 (vec-dot surface_norm light_norm _))
		specular (max 0.0 (vec-dot ref (vec-norm (vec-sub cam_pos surface_pos) _) _))
		specular (fmul specular specular specular specular)
		obj_color (vec-scale obj_color (add (fmul diffuse (sub 1.0 ambient)) ambient) _)
		obj_color (vec-add obj_color (points specular specular specular) _))
	(vec-mul obj_color light_col _))

(defun-bind scene-ray (ray_origin ray_dir)
	(defq l (ray-march ray_origin ray_dir 0.0 clipfar min_distance march_factor))
	(if (ge l clipfar)
		(const (points 0.0 0.0 0.0))
		(progn
			(defq surface_pos (vec-add ray_origin (vec-scale ray_dir l))
				surface_norm (get-normal surface_pos)
				color (lighting surface_pos surface_norm ray_origin))
			(defq i ref_depth r ref_coef)
			(while (and (ge (setq i (dec i)) 0)
						(lt (defq ray_origin surface_pos
								ray_dir (vec-reflect ray_dir surface_norm)
								l (ray-march ray_origin ray_dir (fmul min_distance 2.0)
									clipfar min_distance march_factor)) clipfar))
					(defq surface_pos (vec-add ray_origin (vec-scale ray_dir l))
						surface_norm (get-normal surface_pos)
						color (vec-add (vec-scale color (sub 1.0 r))
								(vec-scale (lighting surface_pos surface_norm ray_origin) r))
						r (fmul r ref_coef)))
			(vec-clamp color 0.0 0.999))))

(defun-bind line (parent w h y)
	(defq w2 (div w 2) h2 (div h 2) x -1
		reply (cat (char (task-mailbox) long_size) (char y int_size)))
	(while (lt (setq x (inc x)) w)
		(defq
			ray_origin (const (points 0 0 -3.0))
			ray_dir (vec-norm (vec-sub
				(points (div (mul (sub x w2) 1.0) w2) (div (mul (sub y h2) 1.0) h2) 0.0)
				ray_origin)))
		(bind '(r g b) (scene-ray ray_origin ray_dir))
		(setq reply (cat reply (char (add (shr b 8) (logand g 0xff00)
			(shl (logand r 0xff00) 8) 0xff000000) int_size)))
		;while does a yield call !
		(while nil))
	(mail-send reply parent))

;read work request or exit
(while (ne 0 (defq parent (get-long (defq msg (mail-mymail)) work_parent_id)))
	(line parent (get-long msg work_width) (get-long msg work_height) (get-long msg work_y)))
