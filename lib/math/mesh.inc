;;;;;;;;;;
; tri mesh
;;;;;;;;;;

(import "lib/math/surface.inc")

;module
(env-push)

(defun Tri (i0 i1 i2 in)
	(list i0 i1 i2 in))

(defun opt-vectors (vectors)
	; (opt-vectors vectors) -> (new_vectors vector_map)
	(defq vec_xmap (xmap 101 (const vec-eql))
		vec_imap (list) new_vecs (list))
	(each (lambda (v)
		(unless (defq i (. vec_xmap :find v))
			(setq i (length new_vecs))
			(. vec_xmap :insert v i)
			(push new_vecs v))
		(push vec_imap i)) vectors)
	(list new_vecs vec_imap))

(defclass Mesh () nil
	; (Mesh) -> mesh
	(def this :verts (list) :norms (list) :tris (list))

	(defmethod :get_verts (this)
		; (. mesh :get_verts) -> verts
		(get :verts this))

	(defmethod :get_norms (this)
		; (. mesh :get_norms) -> norms
		(get :norms this))

	(defmethod :get_tris (this)
		; (. mesh :get_tris) -> tris
		(get :tris this))

	(defmethod :optimise (this)
		; (. mesh :optimise) -> this
		(raise :verts :norms :tris)
		(bind '(new_verts vert_map) (opt-vectors verts))
		(bind '(new_norms norm_map) (opt-vectors norms))
		(setq verts new_verts norms new_norms tris (map (lambda ((i0 i1 i2 in))
			(Tri (elem i0 vert_map) (elem i1 vert_map) (elem i2 vert_map) (elem in norm_map))) tris))
		(lower :verts :norms :tris))
	)

(defclass Mesh-sphere (radius eps) (Mesh)
	; (Mesh-sphere radius eps) -> mesh
	(defq y 1 eps2 (* eps 2) eps2_r (i2r eps2)
		verts (cap (+ (* eps2 (dec eps)) 2) (list))
		norms (cap (* eps eps2) (list))
		tris (cap (+ (* eps2 (- eps 2) 2) (* eps2 2)) (list)))
	;gen verts
	(while (< y eps)
		(defq x 0 ya (/ (* (i2r y) +real_2pi) eps2_r)
			yv (* radius (cos ya)) r (* radius (sin ya)))
		(while (< x eps2)
			(defq xa (/ (* (i2r x) +real_2pi) eps2_r)
				xv (* r (sin xa)) zv (* r (cos xa)))
			(push verts (Vec3-r xv yv zv))
			(setq x (inc x)))
		(setq y (inc y)))
	(push verts (Vec3-r +real_0 radius +real_0) (Vec3-r +real_0 (neg radius) +real_0))
	;gen norms and tris for mid section strips
	(defq y 1)
	(while (< y (dec eps))
		(defq x 0)
		(while (< x eps2)
			(defq i0 (+ x (* (dec y) eps2))
				i1 (+ (% (inc x) eps2) (* (dec y) eps2))
				i2 (+ i0 eps2) i3 (+ i1 eps2)
				v0 (elem i0 verts) v1 (elem i1 verts)
				v2 (elem i2 verts) v3 (elem i3 verts))
			(push tris (Tri i0 i2 i1 (length norms)))
			(push tris (Tri i1 i2 i3 (length norms)))
			(push norms (vec-norm (apply reals (vec-cross-3d (vec-sub v0 v1) (vec-sub v0 v2)))))
			(setq x (inc x)))
		(setq y (inc y)))
	;gen norms and tris for end cap fans
	(defq x 0 y (* eps2 (- eps 2)))
	(while (< x eps2)
		(defq i0 (+ x y) i1 (+ (% (inc x) eps2) y) i2 (dec (length verts))
			v0 (elem i0 verts) v1 (elem i1 verts) v2 (elem i2 verts))
		(push tris (Tri i0 i2 i1 (length norms)))
		(push norms (vec-norm (apply reals (vec-cross-3d (vec-sub v0 v1) (vec-sub v0 v2)))))
		(defq i0 x i1 (% (inc x) eps2) i2 (- (length verts) 2)
			v0 (elem i0 verts) v1 (elem i1 verts) v2 (elem i2 verts))
		(push tris (Tri i0 i1 i2 (length norms)))
		(push norms (vec-norm (apply reals (vec-cross-3d (vec-sub v2 v1) (vec-sub v2 v0)))))
		(setq x (inc x)))
	(each (# (push %0 +real_1)) verts)
	(shuffle tris)
	(lower :verts :norms :tris))

(defclass Mesh-torus (radius_ring radius_body eps) (Mesh)
	; (Mesh-torus radius_ring radius_body eps) -> mesh
	(defq y 0 eps2 (* eps 2) eps_r (i2r eps) eps2_r (i2r eps2)
		verts (cap (* eps eps2) (list))
		norms (cap (* eps eps2) (list))
		tris (cap (* eps eps2 2) (list)))
	;gen verts
	(while (< y eps)
		(defq x 0 ya (/ (* (i2r y) +real_2pi) eps_r)
			yv (* radius_body (cos ya))
			r (+ radius_ring (* radius_body (sin ya))))
		(while (< x eps2)
			(defq xa (/ (* (i2r x) +real_2pi) eps2_r)
				xv (* r (sin xa)) zv (* r (cos xa)))
			(push verts (Vec3-r xv yv zv))
			(setq x (inc x)))
		(setq y (inc y)))
	;gen norms and tris
	(defq y 0)
	(while (< y eps)
		(defq x 0)
		(while (< x eps2)
			(defq i0 (+ x (* y eps2))
				i1 (+ (* y eps2) (% (inc x) eps2))
				i2 (+ x (* (% (inc y) eps) eps2))
				i3 (+ (% (inc x) eps2) (* (% (inc y) eps) eps2))
				v0 (elem i0 verts) v1 (elem i1 verts)
				v2 (elem i2 verts) v3 (elem i3 verts))
			(push tris (Tri i0 i2 i1 (length norms)))
			(push tris (Tri i1 i2 i3 (length norms)))
			(push norms (vec-norm (apply reals (vec-cross-3d (vec-sub v0 v1) (vec-sub v0 v2)))))
			(setq x (inc x)))
		(setq y (inc y)))
	(each (# (push %0 +real_1)) verts)
	(shuffle tris)
	(lower :verts :norms :tris))

(defclass Mesh-iso (iso isolevel) (Mesh)
	; (Mesh-iso iso isolevel) -> mesh
	(bind '(width height depth) (. iso :get_metrics))
	(defq verts (list) norms (list) tris (list)
		width (dec width) height (dec height) depth (dec depth) z -1)
	(while (< (setq z (inc z)) depth)
		(defq y -1)
		(while (< (setq y (inc y)) height)
			(defq x -1)
			(while (< (setq x (inc x)) width)
				(each (lambda ((v0 v1 v2))
						(defq i (length verts))
						(push tris (Tri i (inc i) (+ i 2) (length norms)))
						(push verts (cat v0) (cat v1) (cat v2))
						(push norms (vec-norm (apply reals (vec-cross-3d (vec-sub v0 v1) (vec-sub v0 v2))))))
					(. iso :get_surface x y z isolevel)))))
	(lower :verts :norms :tris)
	(. this :optimise)
	(each (# (push %0 +real_1)) verts)
	(shuffle tris))

;module
(export (penv)
	*class_Mesh* Mesh Mesh?
	*class_Mesh-torus* Mesh-torus Mesh-torus?
	*class_Mesh-sphere* Mesh-sphere Mesh-sphere?
	*class_Mesh-iso* Mesh-iso Mesh-iso?)
(env-pop)
