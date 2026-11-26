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

;the numbers of the bake: the most of a colour's channel; how much
;brighter a vivid colour is; where on the coarse blur the far nebula's
;amber and teal are; how many pixels the coarse blur has; the least of a
;way toward the picture, and how far toward it a way is in its edge; how
;far out a way is, of the box, of its roundness and of the nebula's
;thickness, and of a way not toward it at all; where outside the picture
;is, how far it spreads, and how it thickens; how much it is outside and
;of its thickness; the glow over the sky, of the thickness and toward the
;picture; the galaxy's band, how wide, its core how bright and wide, its
;lanes of dust, how uneven along it, and the scale of its noise; the far
;nebula's cloud, the scale of its noise, how thick, where its edge is and
;how uneven; where its colour is all teal, the scale of the noise of it
;and where in it; and the galaxy's colour
(defq +channel_most (n2d 255) +vivid_bright (n2d 1.2) +amber_v (n2d 0.88) +teal_u (n2d 0.78)
	+teal_v (n2d 0.22) +coarse_pixels (n2d 160) +toward_least (n2d 0.001) +toward_edge (n2d 0.02)
	+out_box (n2d 0.55) +out_round (n2d 0.45) +out_wisp (n2d 0.9) +out_none (n2d 100)
	+outside_from (n2d 0.8) +outside_to (n2d 1.1) +spread_from (n2d 1.05) +spread_to (n2d 1.9)
	+spread_wisp (n2d 1.2) +far_least (n2d 0.55) +far_wisp (n2d 0.6) +glow_wisp (n2d 0.12)
	+glow_toward (n2d 0.75) +band_wide (n2d 0.3) +core_bright (n2d 0.08) +core_wide (n2d 0.09)
	+lanes_least (n2d 0.3) +lanes_deep (n2d 0.7) +band_uneven_least (n2d 0.6) +band_uneven (n2d 0.8)
	+band_scale (n2d 1.9) +cloud_scale (n2d 1.3) +cloud_wisp (n2d 0.9) +cloud_from (n2d 0.42)
	+cloud_to (n2d 0.92) +cloud_edge_wisp (n2d 0.35) +hue_to (n2d 0.75) +hue_scale (n2d 2.2)
	+hue_off_y (n2d 11) +galaxy_red (n2d 0.95) +galaxy_green (n2d 0.88) +galaxy_blue (n2d 0.8))

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
					(push reds (/ (n2d (logand (>> c 16) 0xff)) +channel_most))
					(push greens (/ (n2d (logand (>> c 8) 0xff)) +channel_most))
					(push blues (/ (n2d (logand c 0xff)) +channel_most))) '() 0 w)) '() 0 h)
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
	(map (# (max +double_0 (* +vivid_bright (+ grey (* +double_2 (- %0 grey)))))) rgb))

(defun channel (v)
	; (channel v) -> num
	(max 0 (min 255 (n2i (* v +channel_most)))))

(defun bake ()
	; (bake) -> :t
	(defq picture (canvas-load +source +load_flag_noswap)
		fine (blur-of picture 40 64) coarse (blur-of picture 10 16)
		program (shader-load "apps/demos/space/nebula.shader")
		wisp (shader-vp-func program 'wisp) dust (shader-vp-func program 'dust)
		;the far nebula's two colours, the picture's amber and its teal, their
		;colour made stronger, a blur greys it
		amber (vivid (blur-at coarse +double_1/5 +amber_v))
		teal (vivid (blur-at coarse +teal_u +teal_v))
		;the mean of the picture's colours, the glow over all of the sky
		mean (map! (# (/ (nums-sum %0) +coarse_pixels)) (list coarse) 2 -1)
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
						near (max toward +toward_least)
						x (/ across (* near sx)) y (/ up (* near sy))
						box (max (abs x) (abs y))
						w (wisp (doubles dx dy dz))
						;how far out, rounder than the picture's box the further out, and
						;moved in and out by the nebula's thickness, so its edge is uneven
						out_by (if (> toward +toward_edge)
							(+ (* box +out_box) (* +out_round (sqrt (+ (* x x) (* y y))))
								(* +out_wisp (- w +double_1/2)))
							+out_none)
						;outside it the nebula goes on as the edge has it, then thins out,
						;less far than it is wide, and further in thick ways
						outside (ease +outside_from +outside_to box)
						spread (- +double_1 (ease +spread_from (+ +spread_to (* +spread_wisp w)) out_by))
						far (* spread spread (+ +far_least (* +far_wisp w)))
						glow (* +glow_wisp w (+ +double_1/4 (* +glow_toward (max toward +double_0))))
						u (+ +double_1/2 (* +double_1/2 x)) v (- +double_1/2 (* +double_1/2 y))
						;the galaxy's band, wide and faint with a brighter core, dark
						;lanes of dust in it, uneven along it
						from_band (abs (+ (* dx bx) (* dy by) (* dz bz)))
						lanes (dust (doubles dx dy dz))
						galaxy (* (+ (* +double_1/20 (- +double_1 (ease +double_0 +band_wide from_band)))
								(* +core_bright (- +double_1 (ease +double_0 +core_wide from_band))))
							(+ +lanes_least (* +lanes_deep lanes))
							(+ +band_uneven_least (* +band_uneven (wisp (doubles (+ (* dx +band_scale) +double_4)
								(+ (* dy +band_scale) +double_1) (+ (* dz +band_scale) +double_7))))))
						;the far nebula, a cloud round its way, its edge and its body
						;moved by noise of their own, amber into teal
						w3 (wisp (doubles (+ (* dx +cloud_scale) +double_7) (+ (* dy +cloud_scale) +double_3)
							(+ (* dz +cloud_scale) +double_9)))
						cloud (* +double_1/2 (+ +double_1/4 (* +cloud_wisp w3))
							(ease +cloud_from +cloud_to
								(+ (* dx fx) (* dy fy) (* dz fz) (* +cloud_edge_wisp (- w3 +double_1/2)))))
						hue (ease +double_1/4 +hue_to (wisp (doubles (+ (* dx +hue_scale) +double_1)
							(+ (* dy +hue_scale) +hue_off_y) (+ (* dz +hue_scale) +double_4)))))
					;the fine blur within the picture, the coarse one past it, and
					;between them the one into the other
					(bind '(r g b) (map (lambda (in out) (+ in (* outside (- (* out far) in))))
						(blur-at fine u v) (blur-at coarse u v)))
					(bind '(mr mg mb) mean)
					(bind '(cr cg cb) (map (lambda (a t) (* cloud (+ a (* hue (- t a))))) amber teal))
					(obj-set pixmap (+ +pixmap_pixels (* col 4) (* row stride)) +type_uint
						(+ 0xff000000
							(<< (channel (+ r (* mr glow) cr (* galaxy +galaxy_red))) 16)
							(<< (channel (+ g (* mg glow) cg (* galaxy +galaxy_green))) 8)
							(channel (+ b (* mb glow) cb (* galaxy +galaxy_blue))))))
				'() 0 +map_w))
		'() 0 +map_h)
	(defq stream (file-stream +out +file_open_write))
	;32 bits a pixel, so its dark is not in steps
	(CPM-save out stream 32 :nil :t)
	(stream-flush stream)
	:t)

(time-it "space nebula baked" (bake))
