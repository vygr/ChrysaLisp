;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a picture of a network, made from its model, lib/motion
;
; A column for each layer: the input, a picture of it if its count is a
; square, else a column of dots, each hidden layer a grid of its units,
; and the outputs as bars with their numbers. (nn-view-show) eases each
; unit to how strongly it fires for a sample, and each bar to its chance,
; the one the network takes it for green if that is right, red if not,
; and (nn-view-weights) draws each output's strongest weights from the
; layer before it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././motion/rostrum.inc")
(import "./model.inc")

(defq +nn_view_font "fonts/OpenSans-Regular.ctf" +nn_view_strong 4
	+nn_view_back 0xff12161c +nn_view_off 0xff262c36 +nn_view_on 0xffffb040
	+nn_view_bar 0xff4a6890 +nn_view_right 0xff40c060 +nn_view_wrong 0xffe04848
	+nn_view_text 0xffc8d0dc +nn_view_link 0xff60a0ff)

;a view: (rostrum input_box units bars links model), input_box (x y side
;cell) or :nil, units a list a hidden layer of their figures, bars the
;output figures, links the figures of the strongest weights

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

(defun nn-view (model width height)
	; (nn-view model width height) -> view
	(defq stage (Rostrum width height) layers (nn-layers model) columns (inc (length layers))
		gap (/ width columns) top 40 bottom (- height 50) input_box :nil units (list) bars (list) links (list)
		ins (nn-ins model) side (n2i (sqrt (n2d ins))))
	(nn-view-figure stage (nn-view-rect width height) +nn_view_back 0 0)
	;the input, a square picture or a column of dots
	(cond
		((= (* side side) ins)
			(defq cell (max 1 (/ (min (- gap 30) (- bottom top)) side)) x 16 y (/ (- height (* side cell)) 2))
			(setq input_box (list x y side cell))
			(nn-view-figure stage (nn-view-rect (+ (* side cell) 4) (+ (* side cell) 4)) +nn_view_off (- x 2) (- y 2)))
		(:t (setq input_box :nil)))
	(nn-view-words stage (cat (str ins) " in") 16 (- top 12) 13)
	;each layer, the last as bars
	(each (lambda ((kind lins outs act name &ignore))
			(defq x (+ 16 (* (inc (!)) gap)) final (= (!) (dec (length layers))))
			(cond
				(final
					(defq bar_h (min 24 (/ (- bottom top) outs)) y0 (+ top (/ (- (- bottom top) (* bar_h outs)) 2)))
					(each! (lambda ()
							(defq y (+ y0 (* (!) bar_h)))
							(nn-view-words stage (str (!)) x (+ y (- bar_h 6)) 12)
							(push bars (nn-view-figure stage (nn-view-rect (- gap 50) (- bar_h 4)) +nn_view_bar (+ x 16) y))
							(def (last bars) :fig_sx 0.0))
						'() 0 outs))
				(:t ;a grid of units, as many rows as fit
					(defq rows (min outs (/ (- bottom top) 12)) cols (/ (+ outs (dec rows)) rows)
						cell (min 12 (/ (- gap 30) cols)) y0 (+ top (/ (- (- bottom top) (* rows 12)) 2)) layer (list))
					(each! (lambda ()
							(push layer (nn-view-figure stage (nn-view-rect (- cell 2) 10) +nn_view_off
								(+ x (* (/ (!) rows) cell)) (+ y0 (* (% (!) rows) 12)))))
						'() 0 outs)
					(push units layer)))
			(nn-view-words stage (cat (str outs) " " (rest (str act))) x (- top 12) 13))
		layers)
	;the strongest weights into each output, strokes, placed by (nn-view-weights)
	(times (* (nn-outs model) +nn_view_strong)
		(defq fig (Figure))
		(def fig :fig_stroke 0 :fig_width 1.0)
		(. stage :add_node fig)
		(push links fig))
	(nn-view-words stage (cat (str (nn-params model)) " parameters") 16 (- height 16) 13)
	(list stage input_box units bars links model))

(defun nn-view-unit (figure)
	; (nn-view-unit figure) -> (x y)
	;the middle of a unit's figure
	(list (+ (n2i (get :fig_x figure)) 5) (+ (n2i (get :fig_y figure)) 5)))

(defun nn-view-weights (view net)
	; (nn-view-weights view net) -> view
	;each output's strongest weights from the layer before, lines from
	;those units to its bar, brighter the stronger
	(bind '(stage input_box units bars links model) view)
	(unless (empty? units)
		(defq from (last units) rows (first (last net)) at 0
			top (reduce (lambda (m row) (reduce (# (max %0 %1)) row m)) rows (n2d 0)))
		(each (lambda (row bar)
				(defq best (list))
				;the strongest few, a pass for each
				(times +nn_view_strong
					(defq i -1 w (n2d 0))
					(each (# (and (> %0 w) (not (find (!) best)) (setq i (!) w %0))) row)
					(push best i))
				(each (lambda (i)
						(defq fig (elem-get links at))
						(setq at (inc at))
						(cond
							((>= i 0)
								(bind '(x y) (nn-view-unit (elem-get from i)))
								(. fig :set_outline (list (list :nil (path (n2q x) (n2q y)
									(get :fig_x bar) (+ (get :fig_y bar) 5.0)))))
								(defq a (min 255 (n2i (* (/ (elem-get row i) top) (n2d 200)))))
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
	(bind '(stage input_box units bars links model) view)
	(each (lambda (layer a)
			(defq top (max (reduce (const max) a (n2d 0)) (n2d 0.001)))
			(each (lambda (fig v)
					(defq k (n2i (* (/ (max v (n2d 0)) top) (n2d 255))))
					(. fig :add (Tween :fig_fill (+ 0xff000000 (<< (+ 0x26 (/ (* k (- 0xff 0x26)) 255)) 16)
						(<< (+ 0x2c (/ (* k (- 0xb0 0x2c)) 255)) 8) (+ 0x36 (/ (* k (- 0x40 0x36)) 255))) 0.25)))
				layer a))
		units (slice acts 0 -2))
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
	(bind '(stage input_box &ignore) view)
	(. stage :advance dt)
	(. stage :draw canvas)
	(when (and input_box pixels)
		(bind '(x y side cell) input_box)
		(each! (lambda ()
				(defq v (code pixels 1 (!)))
				(when (> v 0)
					(.-> canvas (:set_color (+ 0xff000000 (* v 0x010101)))
						(:fbox (+ x (* (% (!) side) cell)) (+ y (* (/ (!) side) cell)) cell cell))))
			'() 0 (* side side)))
	view)
