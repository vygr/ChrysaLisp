(import "lib/math/matrix.inc")

(enums +tri 0
	(enum i_v0 i_v1 i_v2 i_n))

(enums +mesh 0
	(enum verts norms tris))

(defun tri (i_v0 i_v1 i_v2 i_n)
	(list i_v0 i_v1 i_v2 i_n))

(defun mesh (verts norms tris)
	(list verts norms tris))

(defun gen-sphere (radius eps)
	; (gen-sphere radius eps) -> mesh
	(defq verts (list) norms (list) tris (list))
	(defq y 1 eps_r (i2r eps))
	;gen verts
	(while (< y (/ eps 2))
		(defq x 0 ya (/ (* (i2r y) +real_2pi) eps_r)
			yv (* radius (cos ya)) r (* radius (sin ya)))
		(while (< x eps)
			(defq xa (/ (* (i2r x) +real_2pi) eps_r)
				xv (* r (sin xa)) zv (* r (cos xa)))
			(push verts (vec3-r xv yv zv))
			(setq x (inc x)))
		(setq y (inc y)))
	;gen norms and tris
	(defq y 1)
	(while (< y (dec (/ eps 2)))
		(defq x 0)
		(while (< x eps)
			(defq i0 (+ x (* (dec y) eps))
				i1 (+ (% (inc x) eps) (* (dec y) eps))
				i2 (+ i0 eps) i3 (+ i1 eps)
				v0 (elem i0 verts) v1 (elem i1 verts)
				v2 (elem i2 verts) v3 (elem i3 verts))
			(push tris (tri i0 i2 i1 (length norms)))
			(push norms (vec-norm (apply reals (vec-cross-3d (vec-sub v0 v1) (vec-sub v0 v2)))))
			(push tris (tri i1 i2 i3 (length norms)))
			(push norms (vec-norm (apply reals (vec-cross-3d (vec-sub v2 v1) (vec-sub v2 v3)))))
			(setq x (inc x)))
		(setq y (inc y)))
	(push verts (vec3-r +real_0 radius +real_0) (vec3-r +real_0 (neg radius) +real_0))
	(each (# (push %0 +real_1)) verts)
	(mesh verts norms tris))
