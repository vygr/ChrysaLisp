;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/motion/comps.inc - the components that change a figure
;
; A component has its turn each tick, (. comp :update figure dt) with dt the
; seconds since the last, and says :t to go on or :nil if it is over. What it
; was told to do when it is over is then done, (then figure comp), which can
; add components or figures, or kill the figure.
;
; Any of them can change any attribute. A number goes by its value, a colour,
; of :fill :stroke, by each of its channels, an outline by a morph.
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
	;an attribute set, and what is kept of what it was made from let go
	(if (find key '(:width :trim :stroke :fill)) (def figure :flat :nil))
	(def figure key val)
	figure)

(defclass Comp (&optional then) :nil
	; (Comp [then]) -> comp
	;then is fired when it is over, (then figure comp)
	(def this :then then)

	(defmethod :update (figure dt)
		; (. comp :update figure dt) -> :t | :nil
		:nil)

	(defmethod :over (figure)
		; (. comp :over figure) -> comp
		(if (defq then (get :then this)) (then figure this))
		this)
	)

(defclass Tween (key to secs &optional ease then wait) (Comp then)
	; (Tween key to secs [ease then wait]) -> comp
	;the attribute goes from what it is when this starts to that value, over
	;those seconds by that ease, after waiting so many seconds
	(def this :key key :to to :secs (n2f secs) :ease (ifn ease (const ease-in-out))
		:wait (n2f (ifn wait 0.0)) :time 0.0 :from :nil :morph :nil)

	(defmethod :update (figure dt)
		; (. tween :update figure dt) -> :t | :nil
		(raise :key :to :secs :ease :wait :time)
		(setq time (+ time dt))
		(def this :time time)
		(cond
			((< time wait) :t)
			(:t ;it has started, it takes what the attribute is now
				(unless (get :from this)
					(def this :from (get key figure))
					(if (eql key :outline) (def this :morph (Morph (get key figure) to))))
				(defq from (get :from this)
					u (if (> secs 0.0) (min 1.0 (/ (- time wait) secs)) 1.0)
					done (>= u 1.0) e (if done 1.0 (ease u)))
				(cond
					((eql key :outline)
						(. figure :set_outline (if done to (morph-at (get :morph this) e))))
					((find key '(:fill :stroke))
						(motion-set figure key (if done to (motion-mix from to e))))
					(:t (motion-set figure key (if done to (+ from (* (- to from) e))))))
				(not done))))
	)

(defclass Wait (secs &optional then) (Comp then)
	; (Wait secs [then]) -> comp
	;nothing for so many seconds, then it is over
	(def this :secs (n2f secs) :time 0.0)

	(defmethod :update (figure dt)
		; (. wait :update figure dt) -> :t | :nil
		(def this :time (+ (get :time this) dt))
		(< (get :time this) (get :secs this)))
	)

(defclass Rate (key rate &optional accel secs then) (Comp then)
	; (Rate key rate [accel secs then]) -> comp
	;the attribute changes by so much a second, and that by so much a second.
	;For ever, or for so many seconds
	(def this :key key :rate (n2f rate) :accel (n2f (ifn accel 0.0)) :secs secs :time 0.0)

	(defmethod :update (figure dt)
		; (. rate :update figure dt) -> :t | :nil
		(raise :key :rate :accel :secs :time)
		(defq now (+ rate (* accel dt)))
		(motion-set figure key (+ (get key figure) (* now dt)))
		(setq time (+ time dt))
		(def this :rate now :time time)
		(or (not secs) (< time (n2f secs))))
	)

(defclass Wave (key size period &optional phase secs then) (Comp then)
	; (Wave key size period [phase secs then]) -> comp
	;the attribute swings about what it was by size, each swing so many
	;seconds. For ever, or for so many seconds and then back where it was
	(def this :key key :size (n2f size) :period (n2f period) :phase (n2f (ifn phase 0.0))
		:secs secs :time 0.0 :base :nil)

	(defmethod :update (figure dt)
		; (. wave :update figure dt) -> :t | :nil
		(raise :key :size :period :phase :secs :time)
		(unless (get :base this) (def this :base (get key figure)))
		(setq time (+ time dt))
		(def this :time time)
		(defq base (get :base this) going (or (not secs) (< time (n2f secs))))
		(motion-set figure key (if going
			(+ base (* size (sin (+ phase (* +fp_2pi (/ time period))))))
			base))
		going)
	)

(defclass Follow (line secs &optional ease then wait) (Comp then)
	; (Follow line secs [ease then wait]) -> comp
	;x and y go along a path, x y x y, over those seconds by that ease, after
	;waiting so many. An ease that goes past the ends stays on the line
	(def this :line line :secs (n2f secs) :ease (ifn ease (const ease-linear))
		:wait (n2f (ifn wait 0.0)) :time 0.0 :total (morph-length line :nil))

	(defmethod :update (figure dt)
		; (. follow :update figure dt) -> :t | :nil
		(raise :line :secs :ease :wait :time :total)
		(setq time (+ time dt))
		(def this :time time)
		(cond
			((< time wait) :t)
			(:t (defq u (if (> secs 0.0) (min 1.0 (/ (- time wait) secs)) 1.0)
					done (>= u 1.0) e (if done 1.0 (max 0.0 (min 1.0 (ease u))))
					part (morph-trim line :nil e) n (length part))
				(def figure :x (elem-get part (- n 2)) :y (elem-get part (dec n)))
				(not done))))
	)

(defclass Tie (other keys &optional offset scale) (Comp)
	; (Tie other keys [offset scale]) -> comp
	;each of these attributes is that of another figure, times scale and
	;plus offset, as long as the other is there. When it goes this is over
	(def this :other (weak-ref other) :keys keys :offset (n2f (ifn offset 0.0))
		:scale (n2f (ifn scale 1.0)))

	(defmethod :update (figure dt)
		; (. tie :update figure dt) -> :t | :nil
		(when (defq other (obj-ref (get :other this)))
			(unless (get :dead other)
				(raise :offset :scale)
				(each (# (motion-set figure %0 (+ offset (* scale (get %0 other))))) (get :keys this))
				:t)))
	)

(defclass Cues (&rest comps) (Comp)
	; (Cues comp ...) -> comp
	;the components each in turn, the next when the last is over. The cue
	;list of the one figure, what it is to do and in what order
	(def this :cues comps :at 0)

	(defmethod :update (figure dt)
		; (. cues :update figure dt) -> :t | :nil
		(raise :cues :at)
		(when (< at (length cues))
			(defq comp (elem-get cues at))
			(unless (. comp :update figure dt)
				(. comp :over figure)
				(setq at (inc at))
				(def this :at at))
			(< at (length cues))))
	)

(defclass Custom (fnc &optional then) (Comp then)
	; (Custom fnc [then]) -> comp
	;a routine, (fnc figure dt), that says :t to go on
	(def this :fnc fnc)

	(defmethod :update (figure dt)
		; (. custom :update figure dt) -> :t | :nil
		((get :fnc this) figure dt))
	)
