;imports
(import 'apps/cubes/app.inc)

;quick debug switch
;; (import 'class/lisp/debug.inc)
;; (defmacro defun-bind (&rest _) `(defun-debug ~_))

(defun-bind fpoly (canvas col x y _)
	;draw a polygon on a canvas
	(canvas-set-color canvas col)
	(canvas-fpoly canvas x y 0 _))

(defun-bind circle (r)
	;cached circle generation
	(defq i (% r 7) k (elem i '(()()()()()()())) p (elem i '(()()()()()()())))
	(cond ((defq i (some (lambda (i) (if (= i r) _)) k)) (elem i p))
		(t (push k r) (elem -2 (push p (list (points-gen-arc 0 0 0 fp_2pi r 0.25 (points))))))))

(defun-bind lighting (c z)
	;very basic attenuation
	(defq r (logand (>> c 16) 0xff) g (logand (>> c 8) 0xff) b (logand c 0xff)
		at (r2f (./ (const (i2r box_size)) z))
		r (* r at) g (* g at) b (* b at))
	(+ 0xff000000 (logand r 0xff0000) (logand (>> g 8) 0xff00) (logand (>> b 16) 0xff)))

(defun-bind clip_verts (hsw hsh verts)
	;clip and project verts
	(reduce (lambda (out (x y z r c))
		(setq z (.+ z (const (i2r (+ (* box_size 2) max_vel)))))
		(when (.> z (const (i2r focal_len)))
			(setq x (./ (.* x hsw) z) y (./ (.* y hsh) z) r (./ (.* r hsw) z))
			(push out (list (r2f (.+ x hsw)) (r2f (.+ y hsh)) z
				(r2f r) (lighting c z)))) out) verts (list)))

(defun-bind render_verts (canvas verts)
	;render circular verts
	(each (lambda ((sx sy z sr c))
		(fpoly canvas c sx sy (circle sr))) verts))

(defun-bind redraw (dlist)
	;redraw layer/s
	(when (/= 0 (logand (tuple-get dlist_mask dlist) 1))
		(defq canvas (tuple-get dlist_layer1_canvas dlist))
		(canvas-fill canvas 0)
		(bind '(sw sh) (view-pref-size canvas))
		(defq hsw (i2r (>> sw 1)) hsh (i2r (>> sh 1)))
		(render_verts canvas
			(sort (lambda (v1 v2)
				(if (.<= (tuple-get vertex_z v1) (tuple-get vertex_z v2)) 1 -1))
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
