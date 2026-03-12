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
	(each (#
		(vector-quant %0 (const (n2r 0.0001)) %0)
		(unless (defq i (. vec_map :find %0))
			(setq i (/ (length new_vecs) part))
			(. vec_map :insert %0 i)
			(push new_vecs %0))
		(push vec_imap i)) (partition vector part))
	(list new_vecs vec_imap))

(defun opt-mesh (verts norms tris)
	; (opt-mesh verts norms tris) -> (new_verts new_norms new_tris)
	(bind '(new_verts vert_map) (opt-vector verts +vec4_size))
	(bind '(new_norms norm_map) (opt-vector norms +vec3_size))
	(defq new_tris (reduce (lambda (%0 (i0 i1 i2 in))
			(push %0
				(elem-get vert_map i0)
				(elem-get vert_map i1)
				(elem-get vert_map i2)
				(elem-get norm_map in)))
		(partition tris +vec4_size) (nums)))
	(list new_verts new_norms new_tris))

(defun gen-norms (verts tris)
	; (gen-norms verts tris) -> (norms new_tris)
	(defq norms (reals) new_tris (nums))
	(each (lambda ((i0 i1 i2 &ignore))
		(push new_tris i0 i1 i2 (/ (length norms) +vec3_size))
		(setq i0 (* i0 +vec4_size) i1 (* i1 +vec4_size) i2 (* i2 +vec4_size))
		(push norms (Normal
			(slice verts i0 (+ i0 +vec3_size))
			(slice verts i1 (+ i1 +vec3_size))
			(slice verts i2 (+ i2 +vec3_size))))) tris)
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
	(defq eps2 (* eps 2) eps2_r (n2r eps2)
		step (/ +real_2pi eps2_r)
		verts (reals) norms (reals) tris (nums)
		x_sins (list) x_coses (list))
	; Pre-calculate longitudinal (horizontal) angles
	; prevents recalculating the same sines and cosines for every latitude ring
	(for 0 eps2
		(defq xa (* (n2r (!)) step))
		(push x_sins (sin xa))
		(push x_coses (cos xa)))
	; gen verts
	(for 1 eps
		(task-slice)
		(defq ya (* (n2r (!)) step) yv (* radius (cos ya)) r (* radius (sin ya)))
		(each! (# (push verts (* r %0) yv (* r %1) +real_1)) (list x_sins x_coses)))
	(push verts +real_0 radius +real_0 +real_1 +real_0 (neg radius) +real_0 +real_1)
	; gen norms and tris for mid section strips
	(for 1 (dec eps)
		(task-slice)
		; Hoist Y-based index out of the inner loop
		(defq y_eps2 (* (dec (!)) eps2))
		(for 0 eps2
			; Avoid modulo division with a simple bounds wrap check
			(when (= (defq xn (inc (!))) eps2) (setq xn 0))
			(defq i0 (+ (!) y_eps2) i1 (+ xn y_eps2)
				i2 (+ i0 eps2) i3 (+ i1 eps2)
				in (/ (length norms) +vec3_size))
			(push tris i0 i1 i2 in i2 i1 i3 in)
			; Scale indices to byte offsets for normal generation
			(setq i0 (* i0 +vec4_size) i1 (* i1 +vec4_size) i2 (* i2 +vec4_size))
			(push norms (Normal
				(slice verts i0 (+ i0 +vec3_size))
				(slice verts i1 (+ i1 +vec3_size))
				(slice verts i2 (+ i2 +vec3_size))))))
	; gen norms and tris for end cap fans
	(defq y (* eps2 (- eps 2)))
	(for 0 eps2
		; Avoid modulo division for the caps as well
		(when (= (defq xn (inc (!))) eps2) (setq xn 0))
		(defq i0 (+ (!) y) i1 (+ xn y)
			i2 (dec (/ (length verts) +vec4_size))
			in (/ (length norms) +vec3_size))
		(push tris i0 i1 i2 in)
		(setq i0 (* i0 +vec4_size) i1 (* i1 +vec4_size) i2 (* i2 +vec4_size))
		(push norms (Normal
			(slice verts i0 (+ i0 +vec3_size))
			(slice verts i1 (+ i1 +vec3_size))
			(slice verts i2 (+ i2 +vec3_size))))
		(defq i0 xn i1 (!)
			i2 (- (/ (length verts) +vec4_size) 2)
			in (/ (length norms) +vec3_size))
		(push tris i0 i1 i2 in)
		(setq i0 (* i0 +vec4_size) i1 (* i1 +vec4_size) i2 (* i2 +vec4_size))
		(push norms (Normal
			(slice verts i0 (+ i0 +vec3_size))
			(slice verts i1 (+ i1 +vec3_size))
			(slice verts i2 (+ i2 +vec3_size)))))
	(lower :verts :norms :tris))

(defclass Mesh-torus (radius_ring radius_body eps) (Mesh)
	; (Mesh-torus radius_ring radius_body eps) -> mesh
	(defq eps2 (* eps 2) eps_r (n2r eps) eps2_r (n2r eps2)
		y_step (/ +real_2pi eps_r) x_step (/ +real_2pi eps2_r)
		verts (reals) norms (reals) tris (nums)
		x_sins (list) x_coses (list))
	; Pre-calculate toroidal (horizontal) angles
	; prevents recalculating the same sines and cosines for every single ring
	(for 0 eps2
		(defq xa (* (n2r (!)) x_step))
		(push x_sins (sin xa))
		(push x_coses (cos xa)))
	; gen verts
	(for 0 eps
		(task-slice)
		(defq ya (* (n2r (!)) y_step) yv (* radius_body (cos ya))
			r (+ radius_ring (* radius_body (sin ya))))
		(each! (# (push verts (* r %0) yv (* r %1) +real_1)) (list x_sins x_coses)))
	; gen norms and tris
	(for 0 eps
		(task-slice)
		; Hoist the Y-based index calculations out of the inner X loop
		(defq y_eps2 (* (!) eps2) yn_eps2 (* (% (inc (!)) eps) eps2))
		(for 0 eps2
			; Avoid an expensive modulo division in the inner loop with a simple reset
			(when (= (defq xn (inc (!))) eps2) (setq xn 0))
			(defq i0 (+ (!) y_eps2)
				i1 (+ xn y_eps2)
				i2 (+ (!) yn_eps2)
				i3 (+ xn yn_eps2)
				in (/ (length norms) +vec3_size))
			(push tris i2 i0 i1 in i2 i1 i3 in)
			; Scale indices to byte offsets for the normal generation
			(setq i0 (* i0 +vec4_size) i1 (* i1 +vec4_size) i2 (* i2 +vec4_size))
			(push norms (Normal
				(slice verts i0 (+ i0 +vec3_size))
				(slice verts i1 (+ i1 +vec3_size))
				(slice verts i2 (+ i2 +vec3_size))))))
	(lower :verts :norms :tris))

;evaluates a single 2D slice of the 3D volume
(defun eval-slice (z)
	(defq slc (cap (* gw gh 2) (list)) pz (elem-get z_points z))
	(for 0 gh
		(defq y (!) py (elem-get y_points y))
		(for 0 gw
			(defq x (!))
			(push slc
				(Vec3-r (elem-get x_points x) py pz)
				(get_scalar_fnc iso x y z))))
	slc)

(defclass Mesh-iso (iso isolevel) (Mesh)
	; (Mesh-iso iso isolevel) -> mesh
	(bind '(gw gh gd) (. iso :get_metrics))
	(defq verts (reals) norms (reals) tris (nums)
		cw (dec gw) ch (dec gh) cd (dec gd)
		get_scalar_fnc (.? iso :get_scalar)
		x_points (list) y_points (list) z_points (list))
	; Precalculate axes coordinates to avoid vector math in the inner loops
	(bind '(cx cy cz) (get :center iso))
	(bind '(sx sy sz) (get :scale iso))
	(for 0 gw (push x_points (* (- (n2r (!)) cx) sx)))
	(for 0 gh (push y_points (* (- (n2r (!)) cy) sy)))
	(for 0 gd (push z_points (* (- (n2r (!)) cz) sz)))
	; March through the Z depth
	(defq slice_z (eval-slice 0))
	(for 0 cd
		(defq slice_z1 (eval-slice (inc (!))))
		(for 0 ch
			(task-slice)
			(defq y (!) i_y0 (* y gw 2) i_y1 (* (inc y) gw 2))
			(for 0 cw
				(defq x (!) ix (* x 2)
					ix1 (* (inc x) 2)
					idx_00 (+ i_y0 ix)
					idx_10 (+ i_y0 ix1)
					idx_01 (+ i_y1 ix)
					idx_11 (+ i_y1 ix1))
				; Extract cached 3D coordinates and scalar values for the 8 corners
				(each (lambda ((v0 v1 v2))
						(defq i (/ (length verts) +vec4_size) in (/ (length norms) +vec3_size))
						(push tris i (inc i) (+ i 2) in)
						(push verts v0 +real_1 v1 +real_1 v2 +real_1)
						(push norms (Normal v0 v1 v2)))
					(iso-surface (list
						(elem-get slice_z1 idx_00)
						(elem-get slice_z1 idx_10)
						(elem-get slice_z idx_10)
						(elem-get slice_z idx_00)
						(elem-get slice_z1 idx_01)
						(elem-get slice_z1 idx_11)
						(elem-get slice_z idx_11)
						(elem-get slice_z idx_01)
						(elem-get slice_z1 (inc idx_00))
						(elem-get slice_z1 (inc idx_10))
						(elem-get slice_z (inc idx_10))
						(elem-get slice_z (inc idx_00))
						(elem-get slice_z1 (inc idx_01))
						(elem-get slice_z1 (inc idx_11))
						(elem-get slice_z (inc idx_11))
						(elem-get slice_z (inc idx_01))) isolevel))))
		; Shift the slice cache forward
		(setq slice_z slice_z1))
	(bind '(verts norms tris) (opt-mesh verts norms tris))
	(lower :verts :norms :tris))

(defclass Mesh-data (num_verts num_norms num_tris data) (Mesh)
	; (Mesh-data num_verts num_norms num_tris data) -> mesh
	(defq verts (reals) norms (reals) tris (nums) i 0)
	(push verts (slice data i (++ i (* num_verts (const (* +long_size +vec4_size))))))
	(push norms (slice data i (++ i (* num_norms (const (* +long_size +vec3_size))))))
	(push tris (slice data i (++ i (* num_tris (const (* +long_size +tri_size))))))
	(lower :verts :norms :tris))

;module
(export-symbols '(+tri_size))
(export-classes '(Mesh Mesh-data Mesh-torus Mesh-sphere Mesh-iso))
(env-pop)
