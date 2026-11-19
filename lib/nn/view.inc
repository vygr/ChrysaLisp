;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a picture of a network, made from its model, lib/motion
;
; A column for each layer: the input, a picture of it if its count is a
; square, else a column of dots, each hidden dense layer a grid of its
; units, each conv or pool layer its planes, a tile each, and the outputs
; as bars with their numbers. (nn-view-show) eases each unit to how
; strongly it fires for a sample, makes each plane's tile a picture of
; it, a grey each number, scaled to the plane's own least and most, and
; each bar to its chance, the one the network takes it for green if that
; is right, red if not, and (nn-view-weights) draws each output's
; strongest weights from the layer before it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././motion/rostrum.inc")
(import "./model.inc")

(defq +nn_view_font "fonts/OpenSans-Regular.ctf" +nn_view_strong 4
	+nn_view_back 0xff12161c +nn_view_off 0xff262c36 +nn_view_on 0xffffb040
	+nn_view_bar 0xff4a6890 +nn_view_right 0xff40c060 +nn_view_wrong 0xffe04848
	+nn_view_text 0xffc8d0dc +nn_view_link 0xff60a0ff)

;a view: (rostrum input_box units bars links model maps), input_box (x y
;side cell) or :nil, units a list a hidden layer of their figures, :nil
;for one of planes, bars the output figures, links the figures of the
;strongest weights, maps a list a layer of planes of (layer places h w
;cell greys), places the (x y) of each tile, greys a nums of a byte each
;number, or :nil till shown

(defun nn-view-rect (w h)
	; (nn-view-rect w h) -> outline
	(list (list :t (path 0.0 0.0 (n2q w) 0.0 (n2q w) (n2q h) 0.0 (n2q h)))))

(defun nn-view-figure (stage outline fill x y)
	; (nn-view-figure stage outline fill x y) -> figure
	(defq fig (Figure))
	(. fig :set_outline outline)
	(def fig :fig_fill fill :fig_x (n2q x) :fig_y (n2q y))
	(. stage :add_node fig)
	fig)

(defun nn-view-words (stage text x y size &optional colour)
	; (nn-view-words stage text x y size [colour]) -> figure
	;words with their left at x and their foot at y
	(nn-view-figure stage (outline-text text +nn_view_font (* size 2)) (ifn colour +nn_view_text) x y))

(defun nn-view-width (text size)
	; (nn-view-width text size) -> pixels
	;how wide (nn-view-words) draws some words, the bounds of the font of
	;twice the size it makes its outlines of
	(first (font-glyph-bounds (create-font +nn_view_font (* size 2)) text)))

(defun nn-view (model width height)
	; (nn-view model width height) -> view
	(defq stage (Rostrum width height) layers (nn-layers model) columns (inc (length layers))
		gap (/ width columns) top 40 bottom (- height 50) input_box :nil units (list) bars (list) links (list)
		maps (list) ins (nn-ins model) side (n2i (sqrt (n2d ins))))
	(nn-view-figure stage (nn-view-rect width height) +nn_view_back 0 0)
	;the input, a square picture or a column of dots
	(cond
		((= (* side side) ins)
			;two a cell at least, if a narrow column has room
			(defq cell (max 1 (/ (min (- gap 30) (- bottom top)) side) (min 2 (/ (- gap 6) side)))
				x 16 y (/ (- height (* side cell)) 2))
			(setq input_box (list x y side cell))
			(nn-view-figure stage (nn-view-rect (+ (* side cell) 4) (+ (* side cell) 4)) +nn_view_off (- x 2) (- y 2)))
		(:t (setq input_box :nil)))
	(nn-view-words stage (str ins) 16 (- top 22) 12)
	(nn-view-words stage "in" 16 (- top 3) 11)
	;each layer, the last as bars
	(each (lambda ((kind lins outs act name w b shape))
			(defq x (+ 16 (* (inc (!)) gap)) final (= (!) (dec (length layers))) title :nil)
			(cond
				(final
					(defq bar_h (min 24 (/ (- bottom top) outs)) y0 (+ top (/ (- (- bottom top) (* bar_h outs)) 2)))
					(each! (lambda ()
							(defq y (+ y0 (* (!) bar_h)))
							(nn-view-words stage (str (!)) x (+ y (- bar_h 6)) 12)
							(push bars (nn-view-figure stage (nn-view-rect (- gap 50) (- bar_h 4)) +nn_view_bar (+ x 16) y))
							(def (last bars) :fig_sx 0.0))
						'() 0 outs))
				((find kind '(:conv :pool))
					;a tile each plane, as big as they all fit, in as few columns
					(bind '(planes h w) (if (eql kind :conv)
						(list (elem-get shape 6) (elem-get shape 7) (elem-get shape 8))
						(list (elem-get shape 0) (elem-get shape 4) (elem-get shape 5))))
					(defq cell (some! (lambda ()
							(defq c (!) per (max 1 (/ (- bottom top) (+ (* h c) 3))))
							(if (<= (* (/ (+ planes (dec per)) per) (+ (* w c) 3)) (- gap 14)) c))
						'() :nil 4 0)
						cell (ifn cell 1) per (max 1 (/ (- bottom top) (+ (* h cell) 3)))
						rows (min planes per) y0 (+ top (/ (- (- bottom top) (* rows (+ (* h cell) 3))) 2))
						places (map! (lambda ()
								(list (+ x (* (/ (!) rows) (+ (* w cell) 3))) (+ y0 (* (% (!) rows) (+ (* h cell) 3)))))
							'() 0 planes))
					(each (lambda ((tx ty))
							(nn-view-figure stage (nn-view-rect (+ (* w cell) 2) (+ (* h cell) 2)) +nn_view_off (dec tx) (dec ty)))
						places)
					(push maps (list (!) places h w cell :nil))
					(push units :nil)
					(setq title (cat (str planes) "@" (str w))))
				(:t ;a grid of units, as many rows as fit
					(defq rows (min outs (/ (- bottom top) 12)) cols (/ (+ outs (dec rows)) rows)
						cell (min 12 (/ (- gap 30) cols)) y0 (+ top (/ (- (- bottom top) (* rows 12)) 2)) layer (list))
					(each! (lambda ()
							(push layer (nn-view-figure stage (nn-view-rect (- cell 2) 10) +nn_view_off
								(+ x (* (/ (!) rows) cell)) (+ y0 (* (% (!) rows) 12)))))
						'() 0 outs)
					(push units layer)))
			;its size, and its activation under it, held off the right edge
			(defq title (ifn title (str outs)) act (if (eql act :none) "" (rest (str act)))
				tx (min x (- width 6 (max (nn-view-width title 12) (nn-view-width act 11)))))
			;pulled left, the activation a line lower, clear of the layer before
			(nn-view-words stage title tx (- top 22) 12)
			(nn-view-words stage act tx (if (< tx x) (+ top 12) (- top 3)) 11))
		layers)
	;the strongest weights into each output, strokes, placed by (nn-view-weights)
	(times (* (nn-outs model) +nn_view_strong)
		(defq fig (Figure))
		(def fig :fig_stroke 0 :fig_width 1.0)
		(. stage :add_node fig)
		(push links fig))
	(nn-view-words stage (cat (str (nn-params model)) " parameters") 16 (- height 16) 13)
	(list stage input_box units bars links model maps))

(defun nn-view-unit (figure)
	; (nn-view-unit figure) -> (x y)
	;the middle of a unit's figure
	(list (+ (n2i (get :fig_x figure)) 5) (+ (n2i (get :fig_y figure)) 5)))

(defun nn-view-weights (view net)
	; (nn-view-weights view net) -> view
	;each output's strongest weights from the layer before, lines from
	;those units to its bar, brighter the stronger
	(bind '(stage input_box units bars links model &ignore) view)
	(when (and (nempty? units) (last units))
		(defq from (last units) rows (first (last net)) at 0
			top (reduce (lambda (m row) (reduce (# (max %0 (n2d %1))) row m)) rows +double_0))
		(each (lambda (row bar)
				;an app's own task, on the node the GUI is on: it is let in
				(task-slice)
				(defq best (list))
				;the strongest few, a pass for each
				(times +nn_view_strong
					(defq i -1 w +double_0)
					(each (# (and (> (n2d %0) w) (not (find (!) best)) (setq i (!) w (n2d %0)))) row)
					(push best i))
				(each (lambda (i)
						(defq fig (elem-get links at))
						(setq at (inc at))
						(cond
							((>= i 0)
								(bind '(x y) (nn-view-unit (elem-get from i)))
								(. fig :set_outline (list (list :nil (path (n2q x) (n2q y)
									(get :fig_x bar) (+ (get :fig_y bar) 5.0)))))
								(defq a (min 255 (n2i (* (/ (n2d (elem-get row i)) top) (const (n2d 200))))))
								(def fig :fig_stroke (+ (<< a 24) (logand +nn_view_link 0xffffff))))
							(:t (def fig :fig_stroke 0))))
					best))
			rows bars))
	view)

(defun nn-view-show (view acts label)
	; (nn-view-show view acts label) -> view
	;the units eased to how strongly each fired for a sample, acts each
	;layer's outputs as (nn-forward) left them, and the bars to the chance
	;of each, the taken one green if it is the label, red if not
	(bind '(stage input_box units bars links model maps) view)
	(each (lambda (layer a)
			(task-slice)
			(when layer
				(defq top (max (reduce (# (max %0 (n2d %1))) a +double_0) (const (n2d 0.001))))
				(each (lambda (fig v)
						(defq k (n2i (* (/ (max (n2d v) +double_0) top) (const (n2d 255)))))
						(. fig :add (Tween :fig_fill (+ 0xff000000 (<< (+ 0x26 (/ (* k (- 0xff 0x26)) 255)) 16)
							(<< (+ 0x2c (/ (* k (- 0xb0 0x2c)) 255)) 8) (+ 0x36 (/ (* k (- 0x40 0x36)) 255))) 0.25)))
					layer a)))
		units (slice acts 0 -2))
	;each plane a grey each number, from its least to its most
	(each (lambda (m)
			(bind '(l places h w &ignore) m)
			(defq a (elem-get acts l) n (* h w) greys (cap (length a) (nums)))
			(each! (lambda ()
					(defq lo (n2d (elem-get a (* (!) n))) hi lo)
					(each! (lambda (v) (setq v (n2d v) lo (min lo v) hi (max hi v))) (list a) (* (!) n) (* (inc (!)) n))
					(defq span (max (- hi lo) (const (n2d 0.0001))))
					(each! (lambda (v) (push greys (n2i (* (/ (- (n2d v) lo) span) (const (n2d 255))))))
						(list a) (* (!) n) (* (inc (!)) n)))
				'() 0 (length places))
			(elem-set m 5 greys))
		maps)
	(defq p (last acts) guess (nn-guess p))
	(each (lambda (bar v)
			(. bar :add (Tween :fig_sx (n2q v) 0.3 (const ease-out)))
			(. bar :add (Tween :fig_fill (cond ((/= (!) guess) +nn_view_bar)
					((= guess label) +nn_view_right) (+nn_view_wrong)) 0.3)))
		bars p)
	view)

(defun nn-view-draw (view canvas dt pixels)
	; (nn-view-draw view canvas dt pixels) -> view
	;the scene gone on by dt seconds and drawn, and the input picture on it,
	;pixels a str of a byte each, if the input is a square
	(bind '(stage input_box units bars links model maps) view)
	(. stage :advance dt)
	(. stage :draw canvas)
	;each plane's tile its greys, the darkest left as the tile is
	(each (lambda ((l places h w cell greys))
			(when greys
				(defq n (* h w))
				(each (lambda ((tx ty))
						(defq at (* (!) n))
						(each! (lambda (k)
								(when (> k 16)
									(defq i (- (!) at))
									(.-> canvas (:set_color (+ 0xff000000 (* k 0x010101)))
										(:fbox (+ tx (* (% i w) cell)) (+ ty (* (/ i w) cell)) cell cell))))
							(list greys) at (+ at n)))
					places)))
		maps)
	(when (and input_box pixels)
		(bind '(x y side cell) input_box)
		(each! (lambda ()
				(defq v (code pixels 1 (!)))
				(when (> v 0)
					(.-> canvas (:set_color (+ 0xff000000 (* v 0x010101)))
						(:fbox (+ x (* (% (!) side) cell)) (+ y (* (/ (!) side) cell)) cell cell))))
			'() 0 (* side side)))
	view)
