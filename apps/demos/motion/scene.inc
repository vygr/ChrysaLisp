;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What the scenes of the Motion demo share. stage is the Rostrum of the moment,
; and restart says the scene is over and to begin again. Sparks, and the
; things a scene that tells a story is made of: words, captions, chips, lines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/motion/rostrum.inc")

(defq +max_figures 500 +font "fonts/OpenSans-Bold.ctf"
	+scene_regular "fonts/OpenSans-Regular.ctf" +scene_mono "fonts/Hack-Regular.ctf"
	+scene_ink 0xffe8ecf0 +scene_dark 0xff283038
	;the outline of a spark, they are all alike
	*spark_outline* (list (list :t (path -4.0 -2.0 4.0 -2.0 0.0 5.0))))

(defun random-fixed (from to)
	;a number between two others
	(+ from (* (- to from) (/ (n2f (. stage :random 1000)) 1000.0))))

(defun spark-at (x y colour)
	;a bit that flies off, spins, fades and goes. Only so many at once
	(when (< (length (. stage :children)) +max_figures)
		(defq fig (Figure "spark"))
		(. fig :set_outline *spark_outline*)
		(def fig :fig_fill colour :fig_x x :fig_y y :fig_z 0.0)
		(. fig :add (Rate :fig_x (random-fixed -160.0 160.0)) (Rate :fig_y (random-fixed -160.0 160.0) 200.0)
			(Rate :fig_rz (random-fixed -12.0 12.0))
			(Tween :fig_alpha 0.0 (random-fixed 0.3 0.8) (const ease-in)
				(lambda (f c) (. f :kill))))
		(. stage :add_node fig)))

;;;;;;;;;;;;;;;;
; things to make
;;;;;;;;;;;;;;;;

(defun scene-text (text size &optional font)
	; (scene-text text size [font]) -> outline
	;the letters made big and brought down, so that they stay smooth drawn
	;at more than the picture's size
	(map (lambda ((closed p))
			(list closed (mat3x2-vec2-mul (const (fixeds 0.5 0.0 0.0 0.0 0.5 0.0)) p p)))
		(outline-text text (ifn font +scene_regular) (* size 2))))

(defun scene-mono (text size)
	; (scene-mono text size) -> outline
	;code, in cells as a vdu has it, made big and brought down as
	;(scene-text) is
	(map (lambda ((closed p))
			(list closed (mat3x2-vec2-mul (const (fixeds 0.5 0.0 0.0 0.0 0.5 0.0)) p p)))
		(outline-mono text +scene_mono (* size 2))))

(defun scene-lines (lines size &optional font)
	; (scene-lines lines size [font]) -> outline
	;lines of words, each in the middle, one under another, about 0 0
	(defq out (list) step (n2f (+ size (/ size 4))) y (* step (* -0.5 (n2f (dec (length lines))))))
	(each (lambda (line)
			(defq m (fixeds 1.0 0.0 0.0 0.0 1.0 y))
			(each (lambda ((closed p)) (push out (list closed (mat3x2-vec2-mul m p p))))
				(outline-centred (scene-text line size font)))
			(setq y (+ y step)))
		lines)
	out)

(defun scene-add (fig &optional parent)
	;a figure on the stage, or in a group
	(. (ifn parent stage) :add_node fig)
	fig)

(defun scene-word (text size x y colour &optional parent font)
	;words in the middle of a place, seen through to start with
	(defq fig (Figure "word"))
	(. fig :set_outline (outline-centred (scene-text text size font)))
	(def fig :fig_fill colour :fig_x x :fig_y y :fig_alpha 0.0)
	(scene-add fig parent))

(defun scene-show (fig &optional secs wait)
	;it comes into view
	(. fig :add (Tween :fig_alpha 1.0 (ifn secs 0.5) (const ease-out) :nil (ifn wait 0.0))))

(defun scene-go (fig &optional secs)
	;it goes out of view, and goes
	(. fig :add (Tween :fig_alpha 0.0 (ifn secs 0.5) (const ease-in) (lambda (f c) (. f :kill)))))

(defun scene-to (fig x y secs &optional fnc then wait)
	;it goes to a place, by two tweens, what is then fired comes of the one
	(. fig :add (Tween :fig_x x secs (ifn fnc (const ease-in-out)) :nil (ifn wait 0.0))
		(Tween :fig_y y secs (ifn fnc (const ease-in-out)) then (ifn wait 0.0))))

(defun scene-size (fig to secs &optional fnc then wait)
	;it goes to a size, both ways
	(. fig :add (Tween :fig_sx to secs (ifn fnc (const ease-in-out)) :nil (ifn wait 0.0))
		(Tween :fig_sy to secs (ifn fnc (const ease-in-out)) then (ifn wait 0.0))))

(defun scene-net (name)
	;a group on the stage with a layer in it for each sort of thing, the later
	;ones drawn over the earlier. What arrives late goes in with its own sort,
	;the mail stays on top of it whenever it came
	(defq net (Figure name))
	(each (lambda (layer) (def net layer (scene-add (Figure "layer") net)))
		'(:lines :picture :chips :mail))
	(scene-add net))

(defun scene-chip (name x y w h colour &optional parent size)
	;a box with a name in it, a group of the two. Its line draws round it
	;and the rest comes up
	(defq fig (Figure "chip") box (Figure "box") label (Figure "label")
		hw (* 0.5 (n2f w)) hh (* 0.5 (n2f h)))
	(. box :set_outline (list (list :t (path-gen-rect (neg hw) (neg hh) hw hh 5.0 5.0 (path)))))
	(def box :fig_fill +scene_dark :fig_stroke colour :fig_width 2.0 :fig_trim 0.0)
	(. box :add (Tween :fig_trim 1.0 0.7 (const ease-in-out)))
	(. label :set_outline (outline-centred (scene-text name (ifn size 13))))
	(def label :fig_fill +scene_ink :fig_alpha 0.0)
	(scene-show label 0.4 0.3)
	(. fig :add_node box)
	(. fig :add_node label)
	(def fig :fig_x x :fig_y y :box box :label label :name_is name)
	(scene-add fig parent))

(defun scene-pulse (fig)
	;a chip that has been given something swells a little
	(unless (get :pulsing fig)
		(def fig :pulsing :t)
		(. fig :add (Cues (Tween :fig_sx 1.25 0.08 (const ease-out)) (Tween :fig_sx 1.0 0.25 (const ease-in-out)))
			(Cues (Tween :fig_sy 1.25 0.08 (const ease-out))
				(Tween :fig_sy 1.0 0.25 (const ease-in-out) (lambda (f c) (def f :pulsing :nil)))))))

(defun scene-arrow (x y x1 y1 colour secs &optional parent)
	;a line that draws itself, with a head on it
	(defq fig (Figure "arrow"))
	(. fig :set_outline (list (list :nil (path x y x1 y1))))
	(def fig :fig_stroke colour :fig_width 2.5 :fig_trim 0.0 :fig_cap2 +cap_arrow)
	(. fig :add (Tween :fig_trim 1.0 secs (const ease-in-out)))
	(scene-add fig parent))

(defun scene-link (ax ay bx by parent)
	;a line that draws itself between two chips, behind them
	(defq fig (Figure "link"))
	(. fig :set_outline (list (list :nil (path ax ay bx by))))
	(def fig :fig_stroke 0xff485058 :fig_width 1.5 :fig_trim 0.0)
	(. fig :add (Tween :fig_trim 1.0 0.8 (const ease-in-out)))
	(scene-add fig parent))

(defun scene-say (lines &optional secs then)
	;what is said at the foot of the picture, what was said goes. So long,
	;then what follows is fired. With no secs it stays till the next is said
	(if (defq old (get :caption stage)) (scene-go old 0.3))
	(defq fig (Figure "caption"))
	(. fig :set_outline (scene-lines lines 17))
	(def fig :fig_fill +scene_ink :fig_x 320.0 :fig_y 448.0 :fig_alpha 0.0)
	(. fig :add (if secs
			(Cues (Tween :fig_alpha 1.0 0.4 (const ease-out) :nil 0.3) (Wait secs then))
			(Tween :fig_alpha 1.0 0.4 (const ease-out) :nil 0.3)))
	(def stage :caption fig)
	(scene-add fig))

(defun scene-title (text colour &optional size)
	;the big word morphs on to the next, and its colour, and its halo's
	;if it has one
	(defq fig (get :title stage))
	(. fig :add (Tween :fig_outline (outline-centred (scene-text text (ifn size 54) +font)) 1.4 (const ease-in-out))
		(Tween :fig_fill colour 1.4))
	(if (/= 0 (get :fig_glow fig))
		(. fig :add (Tween :fig_glow (logior (logand colour 0xffffff) (logand (get :fig_glow fig) 0xff000000)) 1.4))))
