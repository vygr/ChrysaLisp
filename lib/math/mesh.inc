;;;;;;;;;;
; tri mesh
;;;;;;;;;;

(import "./surface.inc")

;module
(env-push)

(enums +tri 0
	(enum i0 i1 i2 in))

(defmacro Tri (i0 i1 i2 in)
	`(nums ,i0 ,i1 ,i2 ,in))

(defun Normal (p0 p1 p2)
	(defq n (apply reals (vector-cross-3d
		(vector-sub p0 p1 +reals_tmp3)
		(vector-sub p0 p2 (const (cat +reals_tmp3))))))
	(if (eql n +reals_zero3) +reals_zero3 (vector-norm n)))

(defun opt-vector (vector part)
	; (opt-vector vector part) -> (new_vector new_indices)
	(defq vec_map (Fmap 101) vec_imap (nums) new_vecs (reals))
	(each (lambda (v)
		(vector-quant v (const (n2r 0.0001)) v)
		(unless (defq i (. vec_map :find v))
			(setq i (/ (length new_vecs) part))
			(. vec_map :insert v i)
			(push new_vecs v))
		(push vec_imap i)) (partition vector part))
	(list new_vecs vec_imap))

(defun opt-mesh (verts norms tris)
	; (opt-mesh verts norms tris) -> (new_verts new_norms new_tris)
	(bind '(new_verts vert_map) (opt-vector verts 4))
	(bind '(new_norms norm_map) (opt-vector norms 3))
	(defq new_tris (reduce (lambda (tris (i0 i1 i2 in))
			(push tris
				(elem-get vert_map i0)
				(elem-get vert_map i1)
				(elem-get vert_map i2)
				(elem-get norm_map in)))
		(partition tris 4) (nums)))
	(list new_verts new_norms new_tris))

(defun gen-norms (verts tris)
	; (gen-norms verts tris) -> (norms new_tris)
	(defq norms (reals) new_tris (nums))
	(each (lambda ((i0 i1 i2 &ignore))
			(push new_tris i0 i1 i2 (/ (length norms) 3))
			(setq i0 (* i0 4) i1 (* i1 4) i2 (* i2 4))
			(push norms (Normal
				(slice verts i0 (+ i0 3))
				(slice verts i1 (+ i1 3))
				(slice verts i2 (+ i2 3))))) tris)
	(list norms new_tris))

(defclass Mesh () :nil
	; (Mesh) -> mesh
	(def this :verts (reals) :norms (reals) :tris (nums))

	(defgetmethod :verts)
	(defgetmethod :norms)
	(defgetmethod :tris)

	(defmethod :normals ()
		; (. mesh :normals) -> mesh
		(raise :verts :tris)
		(bind '(norms tris) (gen-norms verts tris))
		(lower :norms :tris))

	(defmethod :optimise ()
		; (. mesh :optimise) -> mesh
		(raise :verts :norms :tris)
		(bind '(verts norms tris) (opt-mesh verts norms tris))
		(lower :verts :norms :tris))
	)

(defclass Mesh-sphere (radius eps) (Mesh)
	; (Mesh-sphere radius eps) -> mesh
	(defq y 1 eps2 (* eps 2) eps2_r (n2r eps2)
		verts (reals) norms (reals) tris (nums))
	;gen verts
	(while (< y eps)
		(task-slice)
		(defq x 0 ya (/ (* (n2r y) +real_2pi) eps2_r)
			yv (* radius (cos ya)) r (* radius (sin ya)))
		(while (< x eps2)
			(defq xa (/ (* (n2r x) +real_2pi) eps2_r)
				xv (* r (sin xa)) zv (* r (cos xa)))
			(push verts xv yv zv +real_1)
			(++ x))
		(++ y))
	(push verts +real_0 radius +real_0 +real_1 +real_0 (neg radius) +real_0 +real_1)
	;gen norms and tris for mid section strips
	(defq y 1)
	(while (< y (dec eps))
		(task-slice)
		(defq x 0)
		(while (< x eps2)
			(defq i0 (+ x (* (dec y) eps2))
				i1 (+ (% (inc x) eps2) (* (dec y) eps2))
				i2 (+ i0 eps2)
				i3 (+ i1 eps2)
				in (/ (length norms) 3))
			(push tris i0 i1 i2 in i2 i1 i3 in)
			(setq i0 (* i0 4) i1 (* i1 4) i2 (* i2 4))
			(push norms (Normal
				(slice verts i0 (+ i0 3))
				(slice verts i1 (+ i1 3))
				(slice verts i2 (+ i2 3))))
			(++ x))
		(++ y))
	;gen norms and tris for end cap fans
	(defq x 0 y (* eps2 (- eps 2)))
	(while (< x eps2)
		(defq i0 (+ x y)
			i1 (+ (% (inc x) eps2) y)
			i2 (dec (/ (length verts) 4))
			in (/ (length norms) 3))
		(push tris i0 i1 i2 in)
		(setq i0 (* i0 4) i1 (* i1 4) i2 (* i2 4))
		(push norms (Normal
			(slice verts i0 (+ i0 3))
			(slice verts i1 (+ i1 3))
			(slice verts i2 (+ i2 3))))
		(defq i0 (% (inc x) eps2)
			i1 x
			i2 (- (/ (length verts) 4) 2)
			in (/ (length norms) 3))
		(push tris i0 i1 i2 in)
		(setq i0 (* i0 4) i1 (* i1 4) i2 (* i2 4))
		(push norms (Normal
			(slice verts i0 (+ i0 3))
			(slice verts i1 (+ i1 3))
			(slice verts i2 (+ i2 3))))
		(++ x))
	(lower :verts :norms :tris))

(defclass Mesh-torus (radius_ring radius_body eps) (Mesh)
	; (Mesh-torus radius_ring radius_body eps) -> mesh
	(defq y 0 eps2 (* eps 2) eps_r (n2r eps) eps2_r (n2r eps2)
		verts (reals) norms (reals) tris (nums))
	;gen verts
	(while (< y eps)
		(task-slice)
		(defq x 0 ya (/ (* (n2r y) +real_2pi) eps_r)
			yv (* radius_body (cos ya))
			r (+ radius_ring (* radius_body (sin ya))))
		(while (< x eps2)
			(defq xa (/ (* (n2r x) +real_2pi) eps2_r)
				xv (* r (sin xa)) zv (* r (cos xa)))
			(push verts xv yv zv +real_1)
			(++ x))
		(++ y))
	;gen norms and tris
	(defq y 0)
	(while (< y eps)
		(task-slice)
		(defq x 0)
		(while (< x eps2)
			(defq i0 (+ x (* y eps2))
				i1 (+ (* y eps2) (% (inc x) eps2))
				i2 (+ x (* (% (inc y) eps) eps2))
				i3 (+ (% (inc x) eps2) (* (% (inc y) eps) eps2))
				in (/ (length norms) 3))
			(push tris i2 i0 i1 in i2 i1 i3 in)
			(setq i0 (* i0 4) i1 (* i1 4) i2 (* i2 4))
			(push norms (Normal
				(slice verts i0 (+ i0 3))
				(slice verts i1 (+ i1 3))
				(slice verts i2 (+ i2 3))))
			(++ x))
		(++ y))
	(lower :verts :norms :tris))

(defclass Mesh-iso (iso isolevel) (Mesh)
	; (Mesh-iso iso isolevel) -> mesh
	(bind '(width height depth) (. iso :get_metrics))
	(defq verts (reals) norms (reals) tris (nums)
		width (dec width) height (dec height) depth (dec depth) z -1)
	(while (< (++ z) depth)
		(defq y -1)
		(while (< (++ y) height)
			(task-slice)
			(defq x -1)
			(while (< (++ x) width)
				(each (lambda ((v0 v1 v2))
						(defq i (/ (length verts) 4) in (/ (length norms) 3))
						(push tris i (inc i) (+ i 2) in)
						(push verts v0 +real_1 v1 +real_1 v2 +real_1)
						(push norms (Normal v0 v1 v2)))
					(. iso :get_surface x y z isolevel)))))
	(bind '(verts norms tris) (opt-mesh verts norms tris))
	(lower :verts :norms :tris))

(defclass Mesh-data (num_verts num_norms num_tris data) (Mesh)
	; (Mesh-data num_verts num_norms num_tris data) -> mesh
	(defq verts (reals) norms (reals) tris (nums) i 0)
	(push verts (slice data i (++ i (* num_verts (const (* +long_size +vec4_size))))))
	(push norms (slice data i (++ i (* num_norms (const (* +long_size +vec3_size))))))
	(push tris (slice data i (++ i (* num_tris (const (* +long_size +tri_size))))))
	(lower (:verts verts :norms norms :tris tris)))

;module
(export-symbols '(+tri_size))
(export-classes '(Mesh Mesh-data Mesh-torus Mesh-sphere Mesh-iso))
(env-pop)