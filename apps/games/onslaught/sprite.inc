;;;;;;;;;;;;;;;;;;;
; base sprite class
;;;;;;;;;;;;;;;;;;;

(defclass Sprite (canvas_l canvas_r width height tag &optional death) (View)
	; (Sprite canvas_l canvas_r width height [death]) -> sprite
	(def this :color 0
		:sp_x 0 :sp_y 0 :sp_w width :sp_h height
		:sp_frame_w width :sp_frame_h height
		:sp_canvas_l canvas_l :sp_canvas_r canvas_r :sp_canvas_tag tag
		:sp_frame 0 :sp_dir -1 :sp_components (list)
		:sp_death (ifn death :nil) :sp_type 0)
	(.-> this (:sp_set_pos 0 0) (:sp_set_size width height))

	(defmethod :sp_set_pos (x y)
		; (. sprite :sp_set_pos x y) -> sprite
		(def this :sp_x x :sp_y y)
		(. this :set_pos (* *zoom* x) (* *zoom* y)))

	(defmethod :sp_get_pos ()
		; (. sprite :sp_get_pos) -> (x y)
		(list (get :sp_x this) (get :sp_y this)))

	(defmethod :sp_set_size (w h)
		; (. sprite :sp_set_size w h) -> sprite
		(def this :sp_w w :sp_h h)
		(. this :set_size (* *zoom* w) (* *zoom* h)))

	(defmethod :sp_get_size ()
		; (. sprite :sp_get_size) -> (w h)
		(list (get :sp_w this) (get :sp_h this)))

	(defmethod :sp_set_bounds (x y w h)
		; (. sprite :sp_set_bounds x y w h) -> sprite
		(def this :sp_x x :sp_y y :sp_w w :sp_h h)
		(. this :set_bounds (* *zoom* x) (* *zoom* y) (* *zoom* w) (* *zoom* h)))

	(defmethod :sp_get_bounds ()
		; (. sprite :sp_get_bounds) -> (x y w h)
		(list (get :sp_x this) (get :sp_y this) (get :sp_w this) (get :sp_h this)))

	(defmethod :sp_get_canvas ()
		; (. sprite :sp_get_canvas) -> canvas
		(defq dir (or (get :sp_dir this) -1)
			cr (get :sp_canvas_r this)
			cl (get :sp_canvas_l this))
		(if (and cr (>= dir 0)) cr cl))

	(defmethod :sp_kill ()
		; (. sprite :sp_kill) -> sprite
		(unless (get :sp_dead this)
			(def this :sp_dead :t :sp_frame -1)
			(when (defq death (get :sp_death this))
				(def this :sp_death :nil)
				(death this))
			(when (get :parent this)
				(. this :sub)))
		this)

	(defmethod :sp_set_frame (f)
		; (. sprite :sp_set_frame f) -> sprite
		(if (= f -1)
			(. this :sp_kill)
			(when (/= f (get :sp_frame this))
				(def this :sp_frame f)))
		this)

	(defmethod :sp_add_component (comp)
		; (. sprite :sp_add_component comp) -> sprite
		(push (get :sp_components this) comp)
		this)

	(defmethod :sp_update ()
		; (. sprite :sp_update) -> sprite
		(if (or (= (get :sp_frame this) -1) (get :sp_dead this))
			(. this :sp_kill)
			(progn
				(each (lambda (comp)
					(unless (or (= (get :sp_frame this) -1) (get :sp_dead this))
						(if (env? comp)
							((get :update comp) this comp)
							(comp this :nil))))
					(get :sp_components this))
				(when (or (= (get :sp_frame this) -1) (get :sp_dead this))
					(. this :sp_kill))))
		this)

	(defmethod :draw ()
		; (. sprite :draw) -> sprite
		(when (/= (defq f (get :sp_frame this)) -1)
			(when (defq c (. this :sp_get_canvas))
				(when (defq texture (getf c +canvas_texture 0))
					(bind '(tid tw th) (texture-metrics texture))
					(bind '(w h) (. this :get_size))
					(defq zoom (or (get :zoom this) 1)
						fw (or (get :sp_frame_w this) 16)
						fh (or (get :sp_frame_h this) 16)
						scaled_fh (* zoom fh)
						scaled_fw (* zoom fw)
						raw_y (* f scaled_fh)
						col (/ raw_y th)
						sy (% raw_y th)
						sx (* col scaled_fw)
						dir (or (get :sp_dir this) -1))
					(when (and (>= dir 0) (get :sp_canvas_r this))
						(setq sx (- tw (+ sx scaled_fw))))
					(. this :ctx_blit tid +argb_white 0 0 w h sx sy))))
		this)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; standard component suite
;;;;;;;;;;;;;;;;;;;;;;;;;;

; movement by vectors

(defun mv-update (this that)
	; this = sprite, that = component instance
	(bind '(x y) (. this :sp_get_pos))
	(defq ax (get :ax that) ay (get :ay that)
		max_vx (get :max_vx that) max_vy (get :max_vy that)
		vx (max (neg max_vx) (min max_vx (+ (get :vx that) ax)))
		vy (max (neg max_vy) (min max_vy (+ (get :vy that) ay))))
	(set that :vx vx :vy vy)
	(. this :sp_set_pos (+ x vx) (+ y vy))
	that)

(defun MV (&optional vx vy ax ay max_vx max_vy)
	; (MV [vx vy ax ay max_vx max_vy]) -> component
	(def (defq that (env 1)) :update (const mv-update)
		:vx (or vx 0) :vy (or vy 0)
		:ax (or ax 0) :ay (or ay 0)
		:max_vx (or max_vx 100) :max_vy (or max_vy 100))
	that)

; movement by vector table

(defun mt-update (this that)
	; this = sprite, that = component instance
	(when (defq table (get :table that))
		(when (> (defq speed (get :speed that)) 0)
			(when (>= (defq count (inc (get :count that))) speed)
				(defq count 0 idx (get :index that))
				(bind '(dx dy) (elem-get table idx))
				(if (defq trk (get :track that))
					(bind '(tx ty) (. trk :sp_get_pos))
					(bind '(tx ty) (. this :sp_get_pos)))
				(. this :sp_set_pos (+ tx dx) (+ ty dy))
				(set that :index (if (= (inc idx) (length table)) 0 (inc idx))))
			(set that :count count)))
	that)

(defun MT (speed table &optional track)
	; (MT speed table [track]) -> component
	(def (defq that (env 1)) :update (const mt-update)
		:speed (or speed 1) :table table :count 0 :index 0
		:track (ifn track :nil track))
	that)

; movement by linear interpolation

(defun ml-update (this that)
	; this = sprite, that = component instance
	(defq speed (get :speed that))
	(when (> speed 0)
		(defq slope (get :slope that) count (get :count that)
			dx (get :dx that) dy (get :dy that)
			d1x (get :d1x that) d2x (get :d2x that)
			d1y (get :d1y that) d2y (get :d2y that))
		(bind '(x y) (. this :sp_get_pos))
		(while (and (> speed 0) (> count 0))
			(-- count)
			(setq slope (- slope dy))
			(if (< slope 0)
				(setq slope (+ slope dx) x (+ x d1x) y (+ y d1y))
				(setq x (+ x d2x) y (+ y d2y)))
			(-- speed))
		(when (<= count 0)
			(setq count -1 x (get :target_x that) y (get :target_y that)))
		(set that :slope slope :count count)
		(. this :sp_set_pos x y))
	that)

(defun init-ml (this that x y x1 y1 &optional speed)
	; (init-ml sprite ml x y x1 y1 [speed]) -> ml
	(when speed (set that :speed speed))
	(. this :sp_set_pos x y)
	(defq dx (- x1 x) dy (- y1 y)
		d1x (cond ((< dx 0) -1) ((> dx 0) 1) (:t 0)) d2x d1x
		d1y (cond ((< dy 0) -1) ((> dy 0) 1) (:t 0)) d2y 0
		abs_dx (abs dx) abs_dy (abs dy) tmpi 0)
	(when (< abs_dx abs_dy)
		(setq d2y d1y d2x 0 tmpi abs_dx abs_dx abs_dy abs_dy tmpi))
	(set that :target_x x1 :target_y y1
		:dx abs_dx :dy abs_dy :d1x d1x :d2x d2x :d1y d1y :d2y d2y
		:count abs_dx :slope (>> abs_dx 1))
	that)

(defun ML (&optional speed)
	; (ML [speed]) -> component
	(def (defq that (env 1)) :update (const ml-update)
		:speed (or speed 0) :count -1 :target_x 0 :target_y 0
		:slope 0 :dx 0 :dy 0 :d1x 0 :d2x 0 :d1y 0 :d2y 0)
	that)

; animation by table

(defun at-update (this that)
	; this = sprite, that = component instance
	(when (defq table (get :table that))
		(when (> (defq speed (get :speed that)) 0)
			(when (>= (defq count (inc (get :count that))) speed)
				(defq count 0 idx (get :index that) f (elem-get table idx))
				(cond
					((= f -1)
						(. this :sp_kill))
					((and (not (get :loop that)) (= (inc idx) (length table)))
						; non-looping animation reached end
						(. this :sp_set_frame f)
						(set that :speed 0 :table :nil :count 0 :index 0))
					(:t
						(. this :sp_set_frame f)
						(set that :index (if (= (inc idx) (length table)) 0 (inc idx))))))
			(set that :count count)))
	that)

(defun AT (speed table &rest opt)
	; (AT speed table [loop]) -> component
	(defq loop (if (empty? opt) :t (first opt)))
	(def (defq that (env 1)) :update (const at-update)
		:speed (or speed 1) :table table :count 0 :index 0
		:loop (if loop :t :nil))
	that)

; procedural component wrapper

(defun cp-update (this that)
	; this = sprite, that = component instance
	(when (defq proc (get :proc that))
		(proc this that))
	that)

(defun CP (proc)
	; (CP proc) -> component
	(def (defq that (env 1)) :update (const cp-update) :proc proc)
	that)

; collision detection

(defun sprite-collide (layer x y w h type)
	; (sprite-collide layer x y w h type) -> sprite | :nil
	(defq xw (+ x w) yh (+ y h))
	(some (lambda (sp)
		(when (and (Sprite? sp)
				   (/= (get :sp_frame sp) -1)
				   (not (get :sp_dead sp))
				   (or (= type -1) (/= 0 (logand (or (get :sp_type sp) 0) type))))
			(bind '(tx ty tw th) (. sp :sp_get_bounds))
			(if (and (< x (+ tx tw)) (< tx xw)
					 (< y (+ ty th)) (< ty yh))
				sp)))
		(cat (. layer :children))))

(defun cl-update (this that)
	; this = sprite, that = component instance
	(when (and (defq lyr (get :layer that))
			   (/= (get :sp_frame this) -1)
			   (not (get :sp_dead this)))
		(bind '(x y w h) (. this :sp_get_bounds))
		(when (defq hit (sprite-collide lyr x y w h (get :type that)))
			(defq tbl (get :table that))
			(cond
				((list? tbl)
					(when (defq idx (log2 (or (get :sp_type hit) 0)))
						(when (< -1 idx (length tbl))
							(when (defq handler (elem-get tbl idx))
								(handler this hit)))))
				(tbl
					(tbl this hit)))))
	that)

(defun CL (layer type table)
	; (CL layer type table) -> component
	(def (defq that (env 1)) :update (const cl-update)
		:layer layer :type (ifn type -1 type) :table table)
	that)

; offscreen killing

(defun offscreen-update (this &optional that)
	; this = sprite, that = component instance
	(bind '(x y w h) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< x (neg w)) (< y (neg h)) (>= x ww) (>= y wh))
		(. this :sp_kill))
	this)
