;imports
(import 'lib/math/math.inc)
(import 'apps/bubbles/app.inc)

(defun-bind fpoly (canvas col x y _)
	;draw a polygon on a canvas
	(canvas-set-color canvas col)
	(canvas-fpoly canvas x y 0 _))

(defun-bind circle (r)
	;cached circle generation, quantised to 1/4 pixel
	(defq r (* (floor (* r 4.0)) 0.25) i (% (logior r) 13)
		k (elem i '(()()()()()()()()()()()()())) p (elem i '(()()()()()()()()()()()()())))
	(cond ((defq i (some (lambda (i) (if (= i r) _)) k)) (elem i p))
		(t (push k r) (elem -2 (push p (list
			(path-gen-arc 0.0 0.0 0.0 (const fp_2pi) r 0.25 (path))))))))

(defun-bind lighting ((r g b) z)
	;very basic attenuation
	(defq at (/ (const (i2n box_size)) z) r (* r at) g (* g at) b (* b at))
	(+ 0xd0000000
		(<< (n2i (* r (const (i2n 0xff)))) 16)
		(<< (n2i (* g (const (i2n 0xff)))) 8)
		(n2i (* b (const (i2n 0xff))))))

(defun-bind clip_verts (hsw hsh verts)
	;clip and project verts
	(reduce (lambda (out ((x y z) _ r c))
		(setq z (+ z (const (i2n (+ (* box_size 2) max_vel)))))
		(when (> z (const (i2n focal_len)))
			(defq v (vec x y z) w (/ hsw z) h (/ hsh z))
			(bind '(sx sy sz) (vec-add v (vec-scale (vec-norm
				(vec-add v (vec-sub (elem +dlist_light_pos+ dlist) v))) r)))
			(defq x (+ (* x h) hsw) y (+ (* y h) hsh) r (* r h)
				sx (+ (* sx h) hsw) sy (+ (* sy h) hsh))
			(push out (list (vec-n2f x y z) (vec-n2f sx sy) (n2f r)
				(lighting c z) (lighting (const (vec-i2n 1 1 1)) z)))) out)
		verts (cap (length verts) (list))))

(defun-bind render_verts (canvas verts)
	;render circular verts
	(each (lambda (((x y z) (sx sy) r c sc))
		(fpoly canvas c x y (circle r))
		(fpoly canvas sc sx sy (circle (* r 0.2)))) verts))

(defun-bind redraw (dlist)
	;redraw layer/s
	(when (/= 0 (logand (elem +dlist_mask+ dlist) 1))
		(defq canvas (elem +dlist_layer1_canvas+ dlist))
		(canvas-fill canvas 0)
		(bind '(sw sh) (view-pref-size canvas))
		(defq hsw (i2n (>> sw 1)) hsh (i2n (>> sh 1)))
		(render_verts canvas
			(sort (lambda (v1 v2) (if (<= (elem -2 (elem 0 v1)) (elem -2 (elem 0 v2))) 1 -1))
				(clip_verts hsw hsh (elem +dlist_layer1_verts+ dlist))))
		(canvas-swap canvas))
	(elem-set +dlist_mask+ dlist 0))

(defun-bind main ()
	;read args from parent (shared dlist tuple)
	(defq dlist (mail-read (task-mailbox)))
	;until quit
	(until (mail-poll (array (task-mailbox)))
		(redraw dlist)
		(task-sleep (elem +dlist_rate+ dlist))))
