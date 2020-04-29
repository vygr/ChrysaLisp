;imports
(import 'apps/cubes/app.inc)

(defun-bind fpoly (canvas col x y _)
	;draw a polygon on a canvas
	(canvas-set-color canvas col)
	(canvas-fpoly canvas x y 0 _))

(defun-bind circle (r)
	;cached circle generation
	(defq i (% (logior r) 7) k (elem i '(()()()()()()())) p (elem i '(()()()()()()())))
	(cond ((defq i (some (lambda (i) (if (= i r) _)) k)) (elem i p))
		(t (push k r) (elem -2 (push p (list
			(path-gen-arc 0.0 0.0 0.0 (fixed fp_2pi) r 0.25 (path))))))))

(defun-bind lighting (c z)
	;very basic attenuation
	(defq r (logand (>> c 16) 0xff) g (logand (>> c 8) 0xff) b (logand c 0xff)
		at (n2i (* (/ (const (i2n box_size)) z) (const (i2n (<< 1 fp_shift)))))
		r (* r at) g (* g at) b (* b at))
	(+ 0xff000000 (logand r 0xff0000) (logand (>> g 8) 0xff00) (logand (>> b 16) 0xff)))

(defun-bind clip_verts (hsw hsh verts)
	;clip and project verts
	(reduce (lambda (out ((x y z) _ r c))
		(setq z (+ z (const (i2n (+ (* box_size 2) max_vel)))))
		(when (> z (const (i2n focal_len)))
			(setq x (/ (* x hsw) z) y (/ (* y hsh) z) r (/ (* r hsw) z))
			(push out (list (vecf (n2f (+ x hsw)) (n2f (+ y hsh)) (n2f z))
				(n2f r) (lighting c z)))) out) verts (list)))

(defun-bind render_verts (canvas verts)
	;render circular verts
	(each (lambda (((x y z) r c))
		(fpoly canvas c x y (circle r))) verts))

(defun-bind redraw (dlist)
	;redraw layer/s
	(when (/= 0 (logand (tuple-get dlist_mask dlist) 1))
		(defq canvas (tuple-get dlist_layer1_canvas dlist))
		(canvas-fill canvas 0)
		(bind '(sw sh) (view-pref-size canvas))
		(defq hsw (i2n (>> sw 1)) hsh (i2n (>> sh 1)))
		(render_verts canvas
			(sort (lambda (((_ _ z1) _ _) ((_ _ z2) _ _)) (if (<= z1 z2) 1 -1))
				(clip_verts hsw hsh (tuple-get dlist_layer1_verts dlist))))
		(canvas-swap canvas))
	(tuple-set dlist_mask dlist 0))

(defun-bind main ()
	;read args from parent (shared dlist tuple)
	(defq dlist (mail-read (task-mailbox)))
	;until quit
	(until (mail-poll (array (task-mailbox)))
		(redraw dlist)
		(task-sleep (tuple-get dlist_rate dlist))))
