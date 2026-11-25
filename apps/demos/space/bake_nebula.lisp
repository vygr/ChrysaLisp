;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's nebula, made once and kept: data/nebula.cpm, the map of all of
; the sky that the sky shader reads behind the Pillars and their stars. Run
; it again after a change to where the Pillars are, sky.inc, or to the
; picture; the stars and the sounds are bakes of their own:
;
;	lisp -r (import "apps/demos/space/bake_nebula.lisp")
;
; The picture is made very small, a blur of it, and each way of the sky
; given the colour of the picture where that way meets it, at its edge if
; it is outside it: so the picture's own nebula is carried out past its edges
; in their colours, thinning out into the dark, more in some ways than
; others, in clouds and strands, nebula.shader, and a faint glow of its
; colours over all of the sky, more on the side the Pillars are. Within
; the picture it is the blur of it, which the picture itself is laid over.
; Round all of the sky the galaxy's band, its glow and the dark lanes of
; its dust, and on the far side from the Pillars a nebula of their own
; colours, so no side of the sky is bare.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/lisp.inc")
(import "lib/image/cpm.inc")
(import "lib/gpu/shader.inc")
(import "lib/gpu/vp.inc")
(import "apps/demos/space/sky.inc")


;the map's size, twice as wide as it is high, the picture, and the map
;made. The picture is made two blurs: a fine one, what the map has where
;the picture is, behind it, and a coarse one, of the colours its nebula is
;carried out in, so they are not streaks of the edge's detail
(defq +map_w 512 +map_h 256
	+source "apps/demos/space/data/pillars.cpm" +out "apps/demos/space/data/nebula.cpm")

(defun ease (lo hi x)
	; (ease lo hi x) -> double
	;0 below lo, 1 above hi, and smooth between, as the shaders' ease
	(defq t (max +double_0 (min +double_1 (/ (- x lo) (- hi lo)))))
	(* t t (- +double_3 (* +double_2 t))))

(defun blur-of (canvas w h)
	; (blur-of canvas w h) -> (w h reds greens blues)
	;the picture made w by h, in two steps so each pixel is the mean of all
	;it covers, each colour a doubles of 0 to 1, a row at a time
	(defq mid (Canvas (* w 6) (* h 6) 1) small (Canvas w h 1))
	(. mid :resize canvas)
	(. small :resize mid)
	(defq pixmap (getf small +canvas_pixmap 0) stride (getf pixmap +pixmap_stride 0)
		reds (doubles) greens (doubles) blues (doubles))
	(each! (lambda () (defq y (!))
			(each! (lambda ()
					(defq c (obj-get pixmap (+ +pixmap_pixels (* (!) 4) (* y stride)) +type_uint))
					(push reds (/ (n2d (logand (>> c 16) 0xff)) (const (n2d 255))))
					(push greens (/ (n2d (logand (>> c 8) 0xff)) (const (n2d 255))))
					(push blues (/ (n2d (logand c 0xff)) (const (n2d 255))))) '() 0 w)) '() 0 h)
	(list w h reds greens blues))

(defun blur-at (blur u v)
	; (blur-at blur u v) -> (r g b)
	;the blur's colour at a place on it, 0 to 1 across and down, the four
	;pixels round it mixed by how near it is to each
	(bind '(w h &ignore) blur)
	(defq fx (max +double_0 (min (n2d (dec w)) (- (* u (n2d w)) +double_1/2)))
		fy (max +double_0 (min (n2d (dec h)) (- (* v (n2d h)) +double_1/2)))
		x0 (n2i fx) y0 (n2i fy) x1 (min (inc x0) (dec w)) y1 (min (inc y0) (dec h))
		tx (- fx (n2d x0)) ty (- fy (n2d y0))
		a (+ x0 (* y0 w)) b (+ x1 (* y0 w)) c (+ x0 (* y1 w)) d (+ x1 (* y1 w)))
	(map! (lambda (ch)
			(defq top (+ (elem-get ch a) (* tx (- (elem-get ch b) (elem-get ch a))))
				bottom (+ (elem-get ch c) (* tx (- (elem-get ch d) (elem-get ch c)))))
			(+ top (* ty (- bottom top))))
		(list blur) 2 -1))

(defun vivid (rgb)
	; (vivid (r g b)) -> (r g b)
	;a colour made twice as far from its grey, and a little brighter
	(defq grey (/ (reduce (const +) rgb +double_0) +double_3))
	(map (# (max +double_0 (* (const (n2d 1.2)) (+ grey (* +double_2 (- %0 grey)))))) rgb))

(defun channel (v)
	; (channel v) -> num
	(max 0 (min 255 (n2i (* v (const (n2d 255)))))))

(defun bake ()
	; (bake) -> :t
	(defq picture (canvas-load +source +load_flag_noswap)
		fine (blur-of picture 40 64) coarse (blur-of picture 10 16)
		program (shader-load "apps/demos/space/nebula.shader")
		wisp (shader-vp-func program 'wisp) dust (shader-vp-func program 'dust)
		;the far nebula's two colours, the picture's amber and its teal, their
		;colour made stronger, a blur greys it
		amber (vivid (blur-at coarse (const (n2d 0.2)) (const (n2d 0.88))))
		teal (vivid (blur-at coarse (const (n2d 0.78)) (const (n2d 0.22))))
		;the mean of the picture's colours, the glow over all of the sky
		mean (map! (# (/ (nums-sum %0) (const (n2d 160)))) (list coarse) 2 -1)
		out (Canvas +map_w +map_h 1) pixmap (getf out +canvas_pixmap 0) stride (getf pixmap +pixmap_stride 0))
	(bind '(px py pz) pillars_at)
	(bind '(ax ay az) pillars_across)
	(bind '(ux uy uz) pillars_up)
	(bind '(sx sy) pillars_size)
	(bind '(bx by bz) band_pole)
	(bind '(fx fy fz) far_at)
	(each! (lambda ()
			(defq row (!) lat (* +double_pi (- +double_1/2 (/ (+ (n2d row) +double_1/2) (n2d +map_h))))
				cl (cos lat) sl (sin lat))
			(each! (lambda ()
					(defq col (!) lon (* +double_2pi (- (/ (+ (n2d col) +double_1/2) (n2d +map_w)) +double_1/2))
						dx (* cl (sin lon)) dy sl dz (* (neg cl) (cos lon))
						toward (+ (* dx px) (* dy py) (* dz pz))
						across (+ (* dx ax) (* dy ay) (* dz az)) up (+ (* dx ux) (* dy uy) (* dz uz))
						;across and up on the picture, its edge at 1; a way not toward
						;it is far outside it, its colour that of the edge it is
						;nearest
						near (max toward (const (n2d 0.001)))
						x (/ across (* near sx)) y (/ up (* near sy))
						box (max (abs x) (abs y))
						w (wisp (doubles dx dy dz))
						;how far out, rounder than the picture's box the further out, and
						;moved in and out by the nebula's thickness, so its edge is uneven
						out_by (if (> toward (const (n2d 0.02)))
							(+ (* box (const (n2d 0.55))) (* (const (n2d 0.45)) (sqrt (+ (* x x) (* y y))))
								(* (const (n2d 0.9)) (- w +double_1/2)))
							(const (n2d 100)))
						;outside it the nebula goes on as the edge has it, then thins out,
						;less far than it is wide, and further in thick ways
						outside (ease (const (n2d 0.8)) (const (n2d 1.1)) box)
						spread (- +double_1 (ease (const (n2d 1.05)) (+ (const (n2d 1.9)) (* (const (n2d 1.2)) w)) out_by))
						far (* spread spread (+ (const (n2d 0.55)) (* (const (n2d 0.6)) w)))
						glow (* (const (n2d 0.12)) w (+ (const (n2d 0.25)) (* (const (n2d 0.75)) (max toward +double_0))))
						u (+ +double_1/2 (* +double_1/2 x)) v (- +double_1/2 (* +double_1/2 y))
						;the galaxy's band, wide and faint with a brighter core, dark
						;lanes of dust in it, uneven along it
						from_band (abs (+ (* dx bx) (* dy by) (* dz bz)))
						lanes (dust (doubles dx dy dz))
						galaxy (* (+ (* (const (n2d 0.05)) (- +double_1 (ease +double_0 (const (n2d 0.30)) from_band)))
								(* (const (n2d 0.08)) (- +double_1 (ease +double_0 (const (n2d 0.09)) from_band))))
							(+ (const (n2d 0.3)) (* (const (n2d 0.7)) lanes))
							(+ (const (n2d 0.6)) (* (const (n2d 0.8)) (wisp (doubles (+ (* dx (const (n2d 1.9))) (const (n2d 4)))
								(+ (* dy (const (n2d 1.9))) (const (n2d 1))) (+ (* dz (const (n2d 1.9))) (const (n2d 7))))))))
						;the far nebula, a cloud round its way, its edge and its body
						;moved by noise of their own, amber into teal
						w3 (wisp (doubles (+ (* dx (const (n2d 1.3))) (const (n2d 7))) (+ (* dy (const (n2d 1.3))) (const (n2d 3)))
							(+ (* dz (const (n2d 1.3))) (const (n2d 9)))))
						cloud (* (const (n2d 0.5)) (+ (const (n2d 0.25)) (* (const (n2d 0.9)) w3))
							(ease (const (n2d 0.42)) (const (n2d 0.92))
								(+ (* dx fx) (* dy fy) (* dz fz) (* (const (n2d 0.35)) (- w3 +double_1/2)))))
						hue (ease (const (n2d 0.25)) (const (n2d 0.75)) (wisp (doubles (+ (* dx (const (n2d 2.2))) +double_1)
							(+ (* dy (const (n2d 2.2))) (const (n2d 11))) (+ (* dz (const (n2d 2.2))) (const (n2d 4)))))))
					;the fine blur within the picture, the coarse one past it, and
					;between them the one into the other
					(bind '(r g b) (map (lambda (in out) (+ in (* outside (- (* out far) in))))
						(blur-at fine u v) (blur-at coarse u v)))
					(bind '(mr mg mb) mean)
					(bind '(cr cg cb) (map (lambda (a t) (* cloud (+ a (* hue (- t a))))) amber teal))
					(obj-set pixmap (+ +pixmap_pixels (* col 4) (* row stride)) +type_uint
						(+ 0xff000000
							(<< (channel (+ r (* mr glow) cr (* galaxy (const (n2d 0.95))))) 16)
							(<< (channel (+ g (* mg glow) cg (* galaxy (const (n2d 0.88))))) 8)
							(channel (+ b (* mb glow) cb (* galaxy (const (n2d 0.80))))))))
				'() 0 +map_w))
		'() 0 +map_h)
	(defq stream (file-stream +out +file_open_write))
	;32 bits a pixel, so its dark is not in steps
	(CPM-save out stream 32 :nil :t)
	(stream-flush stream)
	:t)

(time-it "space nebula baked" (bake))
