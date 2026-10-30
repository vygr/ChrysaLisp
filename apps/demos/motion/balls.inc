;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Balls: figures that look after themselves. Nothing says what happens when:
; balls drift, walls turn them, two that meet flash and throw sparks, the
; pointer splits one, a word morphs on and each morph it finishes sets off a
; ring. A figure with no shape keeps the numbers up.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;what a figure can be, the bits of :fig_kinds and :fig_meets
(bits +kind 0
	(bit wall ball pointer))

(defq +max_balls 40 +min_balls 12
	+words ''("Games" "VP" "Taos" "ChrysaLisp")
	+colours ''(0xffe0a040 0xff40a0e0 0xff60c060 0xffe06080 0xffa070e0)
	word_at 0 words :nil
	;the outlines that are all alike
	*ring_outline* (list (list :t (path-gen-ellipse 0.0 0.0 100.0 100.0 (path))))
	*pointer_outline* (list (list :t (path -3.0 -3.0 3.0 -3.0 3.0 3.0 -3.0 3.0))))

(defun set-speed (fig vx vy)
	;how fast it drifts. A Rate on each of x and y, kept on the figure
	(def (get :rate_x fig) 'rate vx)
	(def (get :rate_y fig) 'rate vy))

(defun count-kind (kind)
	;how many of a kind are on the stage now
	(reduce (lambda (n fig) (if (= 0 (logand kind (get :fig_kinds fig))) n (inc n)))
		(. stage :children) 0))

(defun ball-hit-wall (this wall)
	;turned back, toward the middle of the picture
	(bind '(sx sy) (get :axis wall))
	(defq comp_x (get :rate_x this) comp_y (get :rate_y this))
	(if (/= sx 0.0) (def comp_x 'rate (* sx (abs (get 'rate comp_x)))))
	(if (/= sy 0.0) (def comp_y 'rate (* sy (abs (get 'rate comp_y))))))

(defun ball-hit-ball (this other)
	;it goes off away from the other as fast as it was going, it flashes and
	;comes back to its colour. Whichever is first of the two throws the sparks
	(defq dx (- (get :fig_x this) (get :fig_x other)) dy (- (get :fig_y this) (get :fig_y other))
		len (max 1.0 (sqrt (+ (* dx dx) (* dy dy))))
		vx (get 'rate (get :rate_x this)) vy (get 'rate (get :rate_y this))
		speed (max 40.0 (sqrt (+ (* vx vx) (* vy vy)))))
	(set-speed this (* speed (/ dx len)) (* speed (/ dy len)))
	(. this :add (Cues (Tween :fig_fill 0xffffffff 0.08 (const ease-out))
		(Tween :fig_fill (get :colour this) 0.4 (const ease-in))))
	(when (< (get :fig_id this) (get :fig_id other))
		(defq x (* 0.5 (+ (get :fig_x this) (get :fig_x other))) y (* 0.5 (+ (get :fig_y this) (get :fig_y other))))
		(times 5 (spark-at x y (get :colour this)))))

(defun ball-hit-pointer (this other)
	;it swells, bursts into sparks, and two smaller are thrown out. A small
	;one only goes
	(unless (get :bursting this)
		(def this :bursting :t :fig_meets 0 :fig_kinds 0)
		(. this :add (Tween :fig_sx 1.5 0.15 (const ease-back))
			(Tween :fig_sy 1.5 0.15 (const ease-back) (lambda (f c)
				(defq x (get :fig_x f) y (get :fig_y f) r (get :radius f))
				(times 12 (spark-at x y (get :colour f)))
				(when (> r 12.0)
					(times 2 (ball-new x y (* r 0.65))))
				(. f :kill))))))

(defun ball-new (x y r)
	;a ball, grown from nothing. It comes on as something it can meet only
	;when it is grown, or two made together would part at once
	(defq fig (Figure "ball") colour (elem-get +colours (random (length +colours)))
		comp_x (Rate :fig_x 0.0) comp_y (Rate :fig_y 0.0))
	(. fig :set_outline (list (list :t (path-gen-ellipse 0.0 0.0 r r (path)))))
	(def fig :fig_fill colour :colour colour :fig_stroke 0xffffffff :fig_width 1.5 :radius r
		:fig_x x :fig_y y :fig_sx 0.0 :fig_sy 0.0 :rate_x comp_x :rate_y comp_y
		:fig_on_enter (lambda (this other)
			(defq kinds (get :fig_kinds other))
			(cond
				((/= 0 (logand kinds +kind_wall)) (ball-hit-wall this other))
				((/= 0 (logand kinds +kind_ball)) (ball-hit-ball this other))
				((/= 0 (logand kinds +kind_pointer)) (ball-hit-pointer this other))))
		;another ball can send it back out while it is still in a wall
		:fig_on_stay (lambda (this other)
			(if (/= 0 (logand (get :fig_kinds other) +kind_wall)) (ball-hit-wall this other))))
	(set-speed fig (random-fixed -120.0 120.0) (random-fixed -120.0 120.0))
	(. fig :add comp_x comp_y (Wave :fig_z 120.0 (random-fixed 3.0 7.0) (random-fixed 0.0 6.0))
		(Tween :fig_sx 1.0 0.5 (const ease-back))
		(Tween :fig_sy 1.0 0.5 (const ease-back) (lambda (f c)
			(def f :fig_kinds +kind_ball :fig_meets (const (logior +kind_wall +kind_ball +kind_pointer))))))
	(. stage :add_node fig))

(defun spawner-look (this comp)
	(if (< (count-kind +kind_ball) +min_balls)
		(ball-new (random-fixed 60.0 580.0) (random-fixed 60.0 420.0) (random-fixed 14.0 34.0)))
	(. this :add (Wait 0.4 spawner-look)))

(defun spawner ()
	;it has no shape, every so often it looks how many balls there are
	(defq fig (Figure "spawner"))
	(. fig :add (Wait 0.4 spawner-look))
	(. stage :add_node fig))

(defun ring-new (colour)
	;drawn round itself, it grows and fades, then goes
	(defq fig (Figure "ring"))
	(. fig :set_outline *ring_outline*)
	(def fig :fig_stroke colour :fig_width 4.0 :fig_trim 0.0 :fig_x 320.0 :fig_y 240.0 :fig_sx 0.8 :fig_sy 0.8)
	(. fig :add (Tween :fig_trim 1.0 0.5 (const ease-out))
		(Tween :fig_sx 3.0 1.4 (const ease-out)) (Tween :fig_sy 3.0 1.4 (const ease-out))
		(Tween :fig_alpha 0.0 1.4 (const ease-in) (lambda (f c) (. f :kill))))
	(. stage :add_node fig))

(defun word-next (this comp)
	;a morph is over. A ring goes out, it rests, then it morphs on to the next
	(ring-new (get :fig_fill this))
	(setq word_at (% (inc word_at) (length words)))
	(. this :add (Cues (Wait 1.2)
		(Tween :fig_outline (elem-get words word_at) 1.4 (const ease-in-out) word-next)))
	(. this :add (Cues (Wait 1.2)
		(Tween :fig_fill (elem-get +colours (% word_at (length +colours))) 1.4))))

(defun word-new ()
	;the word in the middle, and it tilts this way and that
	(defq fig (Figure "word"))
	(. fig :set_outline (elem-get words 0))
	(def fig :fig_fill (first +colours) :fig_x 320.0 :fig_y 240.0)
	(. fig :add (Wave :fig_ry 0.6 7.0) (Wave :fig_rx 0.3 11.0) (Wait 1.0 word-next))
	(. stage :add_node fig))

(defun wall-new (x y x1 y1 sx sy)
	;a wall is only there to be met, its colour has no alpha. axis is the way
	;it turns what meets it
	(defq fig (Figure "wall"))
	(. fig :set_outline (list (list :t (path x y x1 y x1 y1 x y1))))
	(def fig :fig_fill 0x00ffffff :fig_kinds +kind_wall :axis (list sx sy))
	(. stage :add_node fig))

(defun balls-new ()
	;all there is to start with. The rest comes of what these do
	(setq word_at 0)
	(def stage :ros_by_depth :t)
	(unless words
		(setq words (map (# (outline-centred (outline-text %0 +font 96))) +words)))
	(wall-new -40.0 -40.0 680.0 0.0 0.0 1.0)
	(wall-new -40.0 480.0 680.0 520.0 0.0 -1.0)
	(wall-new -40.0 -40.0 0.0 520.0 1.0 0.0)
	(wall-new 640.0 -40.0 680.0 520.0 -1.0 0.0)
	(word-new)
	(spawner)
	;the pointer is a small figure, it meets as any other does
	(def (defq pointer (Figure "pointer")) :fig_fill 0x00ffffff :fig_kinds +kind_pointer :fig_x -100.0 :fig_y -100.0)
	(. pointer :set_outline *pointer_outline*)
	(. stage :add_node pointer)
	(def stage :pointer pointer))

