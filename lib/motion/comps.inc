;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/motion/comps.inc - the components that change a figure
;
; A component is an env of its own and a shared update routine, as in the
; Onslaught game. Each tick its env is pushed, (comp-turn comp figure dt), so
; what it keeps are plain variables for as long as it has its turn, and its
; update says :t to go on or :nil if it is over. What it was told to do when it
; is over is then done, (then figure comp), which can add components or
; figures, or kill the figure.
;
; Any of them can change any attribute. A number goes by its value, a colour,
; of :fig_fill :fig_stroke, by each of its channels, an outline by a morph.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./ease.inc")
(import "./figure.inc")

(defun motion-mix (a b u)
	; (motion-mix a b u) -> colour
	;a part of the way from one 32 bit colour to another, each of its
	;channels. A colour of none is that colour but seen through, so the
	;fade to or from it does not go by black
	(cond
		((= a 0) (setq a (logand b 0xffffff)))
		((= b 0) (setq b (logand a 0xffffff))))
	(defq out 0 sh 24)
	(while (>= sh 0)
		(defq ca (logand (>> a sh) 0xff) cb (logand (>> b sh) 0xff)
			c (n2i (+ (n2f ca) (* u (n2f (- cb ca))) 0.5)))
		(setq out (logior out (<< (max 0 (min 255 c)) sh)) sh (- sh 8)))
	out)

(defun motion-set (figure key val)
	; (motion-set figure key val) -> figure
	;an attribute set, and what is kept of what it was made from let go. A
	;colour has no part in the shape but for being there or not
	(cond
		((find key '(:fig_x :fig_y :fig_z :fig_rx :fig_ry :fig_rz :fig_sx :fig_sy :fig_px :fig_py))
			(def figure :fig_local :nil))
		((find key '(:fig_width :fig_trim))
			(def figure :fig_flat :nil))
		((find key '(:fig_fill :fig_stroke))
			(unless (eql (= val 0) (= (get key figure) 0)) (def figure :fig_flat :nil))))
	(def figure key val)
	figure)

(defun tween-update (figure dt)
	(setq time (+ time dt))
	(cond
		((< time wait) :t)
		(:t ;it has started, it takes what the attribute is now
			(unless from
				(setq from (get key figure))
				(if (eql key :fig_outline) (setq morph (Morph from to))))
			(defq u (if (> secs 0.0) (min 1.0 (/ (- time wait) secs)) 1.0)
				done (>= u 1.0) e (if done 1.0 (ease u)))
			(cond
				((eql key :fig_outline)
					(. figure :set_outline (if done to (morph-at morph e))))
				((find key '(:fig_fill :fig_stroke))
					(motion-set figure key (if done to (motion-mix from to e))))
				(:t (motion-set figure key (if done to (+ from (* (- to from) e))))))
			(not done))))

(defun Tween (key to secs &optional ease then wait)
	; (Tween key to secs [ease then wait]) -> comp
	;the attribute goes from what it is when this starts to that value, over
	;those seconds by that ease, after waiting so many seconds
	(def (defq that (env 1)) :update (const tween-update) 'then then
		'key key 'to to 'secs (n2f secs) 'ease (ifn ease (const ease-in-out))
		'wait (n2f (ifn wait 0.0)) 'time 0.0 'from :nil 'morph :nil)
	that)

(defun wait-update (figure dt)
	(setq time (+ time dt))
	(< time secs))

(defun Wait (secs &optional then)
	; (Wait secs [then]) -> comp
	;nothing for so many seconds, then it is over
	(def (defq that (env 1)) :update (const wait-update) 'then then
		'secs (n2f secs) 'time 0.0)
	that)

(defun rate-update (figure dt)
	(setq rate (+ rate (* accel dt)) time (+ time dt))
	(motion-set figure key (+ (get key figure) (* rate dt)))
	(or (not secs) (< time secs)))

(defun Rate (key rate &optional accel secs then)
	; (Rate key rate [accel secs then]) -> comp
	;the attribute changes by so much a second, and that by so much a second.
	;For ever, or for so many seconds. Its rate can be changed as it goes,
	;(def comp 'rate rate)
	(def (defq that (env 1)) :update (const rate-update) 'then then
		'key key 'rate (n2f rate) 'accel (n2f (ifn accel 0.0))
		'secs (if secs (n2f secs)) 'time 0.0)
	that)

(defun wave-update (figure dt)
	(unless base (setq base (get key figure)))
	(setq time (+ time dt))
	(defq going (or (not secs) (< time secs)))
	(motion-set figure key (if going
		(+ base (* size (sin (+ phase (* +fp_2pi (/ time period))))))
		base))
	going)

(defun Wave (key size period &optional phase secs then)
	; (Wave key size period [phase secs then]) -> comp
	;the attribute swings about what it was by size, each swing so many
	;seconds. For ever, or for so many seconds and then back where it was
	(def (defq that (env 1)) :update (const wave-update) 'then then
		'key key 'size (n2f size) 'period (n2f period) 'phase (n2f (ifn phase 0.0))
		'secs (if secs (n2f secs)) 'time 0.0 'base :nil)
	that)

(defun follow-update (figure dt)
	(setq time (+ time dt))
	(cond
		((< time wait) :t)
		(:t (defq u (if (> secs 0.0) (min 1.0 (/ (- time wait) secs)) 1.0)
				done (>= u 1.0) e (if done 1.0 (max 0.0 (min 1.0 (ease u))))
				part (morph-trim line :nil e) n (length part))
			(def figure :fig_x (elem-get part (- n 2)) :fig_y (elem-get part (dec n)) :fig_local :nil)
			(not done))))

(defun Follow (line secs &optional ease then wait)
	; (Follow line secs [ease then wait]) -> comp
	;x and y go along a path, x y x y, over those seconds by that ease, after
	;waiting so many. An ease that goes past the ends stays on the line
	(def (defq that (env 1)) :update (const follow-update) 'then then
		'line line 'secs (n2f secs) 'ease (ifn ease (const ease-linear))
		'wait (n2f (ifn wait 0.0)) 'time 0.0)
	that)

(defun tie-update (figure dt)
	(when (defq fig (obj-ref other))
		(unless (get :fig_dead fig)
			(each (# (motion-set figure %0 (+ offset (* scale (get %0 fig))))) keys)
			:t)))

(defun Tie (other keys &optional offset scale)
	; (Tie other keys [offset scale]) -> comp
	;each of these attributes is that of another figure, times scale and
	;plus offset, as long as the other is there. When it goes this is over
	(def (defq that (env 1)) :update (const tie-update)
		'other (weak-ref other) 'keys keys 'offset (n2f (ifn offset 0.0))
		'scale (n2f (ifn scale 1.0)))
	that)

(defun cues-update (figure dt)
	(when (< at (length cues))
		(defq comp (elem-get cues at))
		(unless (comp-turn comp figure dt)
			(comp-over comp figure)
			(setq at (inc at)))
		(< at (length cues))))

(defun Cues (&rest comps)
	; (Cues comp ...) -> comp
	;the components each in turn, the next when the last is over. The cue
	;list of the one figure, what it is to do and in what order
	(def (defq that (env 1)) :update (const cues-update) 'cues comps 'at 0)
	that)

(defun custom-update (figure dt)
	(fnc figure dt))

(defun Custom (fnc &optional then)
	; (Custom fnc [then]) -> comp
	;a routine, (fnc figure dt), that says :t to go on
	(def (defq that (env 1)) :update (const custom-update) 'then then 'fnc fnc)
	that)
