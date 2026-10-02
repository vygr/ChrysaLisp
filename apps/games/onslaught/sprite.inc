;;;;;;;;;;;;;;;;;;;;;;;;;;
; standard component suite
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
; movement by vectors
;;;;;;;;;;;;;;;;;;;;;

(defun mv-update (this that)
	; this = sprite, that = component instance
	(bind '(x y) (. this :sp_get_pos))
	(setq vx (max (neg max_vx) (min max_vx (+ vx ax)))
		vy (max (neg max_vy) (min max_vy (+ vy ay))))
	(. this :sp_set_pos (+ x vx) (+ y vy))
	that)

(defun MV (&optional vx vy ax ay max_vx max_vy)
	; (MV [vx vy ax ay max_vx max_vy]) -> component
	(def (defq that (env 1)) :update (const mv-update)
		'vx (ifn vx 0) 'vy (ifn vy 0)
		'ax (ifn ax 0) 'ay (ifn ay 0)
		'max_vx (or max_vx 100) 'max_vy (or max_vy 100))
	that)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; movement by vector table
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mt-update (this that)
	; this = sprite, that = component instance
	(if (and track (or (get :sp_dead track) (= (get :sp_frame track) -1)))
		(. this :sp_kill)
		(when table
			(when (> speed 0)
				(when (>= (++ count) speed)
					(setq count 0)
					(bind '(dx dy) (elem-get table index))
					(bind '(tx ty) (. (or track this) :sp_get_pos))
					(. this :sp_set_pos (+ tx dx) (+ ty dy))
					(setq index (if (= (inc index) (length table)) 0 (inc index)))))))
	that)

(defun MT (speed table &optional track)
	; (MT speed table [track]) -> component
	(def (defq that (env 1)) :update (const mt-update)
		'speed (or speed 1) 'table table 'count 0 'index 0
		'track (ifn track :nil track))
	that)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; movement by linear interpolation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ml-update (this that)
	; this = sprite, that = component instance
	(when (> speed 0)
		(defq spd speed)
		(bind '(x y) (. this :sp_get_pos))
		(while (and (> spd 0) (> count 0))
			(-- count)
			(setq slope (- slope dy))
			(if (< slope 0)
				(setq slope (+ slope dx) x (+ x d1x) y (+ y d1y))
				(setq x (+ x d2x) y (+ y d2y)))
			(-- spd))
		(when (<= count 0)
			(setq count -1 x target_x y target_y))
		(. this :sp_set_pos x y))
	that)

(defun init-ml (this that x y x1 y1 speed)
	; (init-ml sprite ml x y x1 y1 [speed]) -> ml
	(. this :sp_set_pos x y)
	(defq dx (- x1 x) dy (- y1 y)
		d1x (cond ((< dx 0) -1) ((> dx 0) 1) (:t 0)) d2x d1x
		d1y (cond ((< dy 0) -1) ((> dy 0) 1) (:t 0)) d2y 0
		abs_dx (abs dx) abs_dy (abs dy) tmpi 0)
	(when (< abs_dx abs_dy)
		(setq d2y d1y d2x 0 tmpi abs_dx abs_dx abs_dy abs_dy tmpi))
	(set that 'speed speed 'target_x x1 'target_y y1
		'dx abs_dx 'dy abs_dy 'd1x d1x 'd2x d2x 'd1y d1y 'd2y d2y
		'count abs_dx 'slope (>> abs_dx 1))
	that)

(defun ML (&optional speed)
	; (ML [speed]) -> component
	(def (defq that (env 1)) :update (const ml-update)
		'speed (ifn speed 0) 'table :nil 'count -1 'index 0
		'target_x 0 'target_y 0 'slope 0 'dx 0 'dy 0
		'd1x 0 'd2x 0 'd1y 0 'd2y 0)
	that)

;;;;;;;;;;;;;;;;;;;;
; animation by table
;;;;;;;;;;;;;;;;;;;;

(defun at-update (this that)
	; this = sprite, that = component instance
	(when table
		(when (> speed 0)
			(when (>= (++ count) speed)
				(setq count 0)
				(defq f (elem-get table index))
				(cond
					((= f -1)
						(. this :sp_kill))
					((and (not loop) (= (inc index) (length table)))
						; non-looping animation reached end
						(. this :sp_set_frame f)
						(setq speed 0 table :nil count 0 index 0))
					(:t
						(. this :sp_set_frame f)
						(setq index (if (= (inc index) (length table)) 0 (inc index))))))))
	that)

(defun AT (speed table &rest opt)
	; (AT speed table [loop]) -> component
	(defq loop (if (empty? opt) :t (first opt)))
	(def (defq that (env 1)) :update (const at-update)
		'speed (or speed 1) 'table table 'count 0 'index 0
		'loop (if loop :t :nil))
	that)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; procedural component wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cp-update (this that)
	; this = sprite, that = component instance
	(when proc
		(proc this that))
	that)

(defun CP (proc)
	; (CP proc) -> component
	(def (defq that (env 1)) :update (const cp-update)
		'proc proc)
	that)

;;;;;;;;;;;;;;;;;;;;;
; collision detection
;;;;;;;;;;;;;;;;;;;;;

(defun sprite-collide (layer x y w h type &optional ignore)
	; (sprite-collide layer x y w h type [ignore]) -> sprite | :nil
	(defq xw (+ x w) yh (+ y h))
	(some (lambda (sp)
		(when (and (Sprite? sp)
				(nql sp ignore)
				(/= (get :sp_frame sp) -1)
				(not (get :sp_dead sp))
				(or (= type -1) (/= 0 (logand (get :sp_type sp) type))))
			(bind '(tx ty tw th) (. sp :sp_get_bounds))
			(if (and (< x (+ tx tw)) (< tx xw)
					(< y (+ ty th)) (< ty yh))
				sp)))
		(. layer :children)))

(defun cl-update (this that)
	; this = sprite, that = component instance
	(when (and layer
			(/= (get :sp_frame this) -1)
			(not (get :sp_dead this)))
		(bind '(x y w h) (. this :sp_get_bounds))
		(when (defq hit (sprite-collide layer x y w h type this))
			(cond
				((or (func? table) (lambda-func? table))
					(table this hit))
				((list? table)
					(when (defq idx (log2 (get :sp_type hit)))
						(when (< -1 idx (length table))
							(when (defq handler (elem-get table idx))
								(handler this hit))))))))
	that)

(defun CL (layer type table)
	; (CL layer type table) -> component
	(def (defq that (env 1)) :update (const cl-update)
		'layer layer 'table table 'type (ifn type -1 type))
	that)

;;;;;;;;;;;;;;;;;;;
; offscreen killing
;;;;;;;;;;;;;;;;;;;

(defun offscreen-nodeath-update (this &optional that)
	; offscreen update that clears death hook before killing sprite
	(bind '(x y w h) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< x (neg w)) (< y (neg h)) (>= x ww) (>= y wh))
		(def this :sp_death :nil)
		(. this :sp_kill))
	this)

(defun offscreen-update (this &optional that)
	; offscreen update that preserves death hook on kill
	(bind '(x y w h) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< x (neg w)) (< y (neg h)) (>= x ww) (>= y wh))
		(. this :sp_kill))
	this)

;;;;;;;;;;;;;;;;;;;
; base sprite class
;;;;;;;;;;;;;;;;;;;

(defclass Sprite (canvas_l canvas_r width height tag &optional death) (View)
	; (Sprite canvas_l canvas_r width height [death]) -> sprite
	(def this :color 0 :zoom *zoom*
		:sp_x 0 :sp_y 0 :sp_w width :sp_h height
		:sp_frame_w width :sp_frame_h height
		:sp_canvas_l canvas_l :sp_canvas_r canvas_r :sp_canvas_tag tag
		:sp_frame 0 :sp_dir 1 :sp_oy 0 :sp_components (list)
		:sp_death (ifn death :nil death) :sp_type 0 :sp_flags 0 :sp_dead :nil
		:sp_hitpnt 0 :sp_addon :nil)
	(.-> this (:sp_set_pos 0 0) (:sp_set_size width height))

	(defmethod :sp_set_oy (oy)
		; (. sprite :sp_set_oy oy) -> sprite
		(def this :sp_oy oy)
		this)

	(defmethod :sp_get_oy ()
		; (. sprite :sp_get_oy) -> oy
		(get :sp_oy this))

	(defmethod :sp_set_pos (x y)
		; (. sprite :sp_set_pos x y) -> sprite
		(def this :sp_x x :sp_y y :zoom *zoom*)
		(. this :set_pos (* *zoom* x) (* *zoom* y)))

	(defmethod :sp_get_pos ()
		; (. sprite :sp_get_pos) -> (x y)
		(list (get :sp_x this) (get :sp_y this)))

	(defmethod :sp_set_size (w h)
		; (. sprite :sp_set_size w h) -> sprite
		(def this :sp_w w :sp_h h :zoom *zoom*)
		(. this :set_size (* *zoom* w) (* *zoom* h)))

	(defmethod :sp_get_size ()
		; (. sprite :sp_get_size) -> (w h)
		(list (get :sp_w this) (get :sp_h this)))

	(defmethod :sp_set_bounds (x y w h)
		; (. sprite :sp_set_bounds x y w h) -> sprite
		(def this :sp_x x :sp_y y :sp_w w :sp_h h :zoom *zoom*)
		(. this :set_bounds (* *zoom* x) (* *zoom* y) (* *zoom* w) (* *zoom* h)))

	(defmethod :sp_get_bounds ()
		; (. sprite :sp_get_bounds) -> (x y w h)
		(list (get :sp_x this) (get :sp_y this) (get :sp_w this) (get :sp_h this)))

	(defmethod :sp_get_canvas ()
		; (. sprite :sp_get_canvas) -> canvas
		(defq dir (get :sp_dir this)
			cr (get :sp_canvas_r this)
			cl (get :sp_canvas_l this))
		(if (and cr (>= dir 0)) cr cl))

	(defmethod :sp_kill ()
		; (. sprite :sp_kill) -> sprite
		(unless (get :sp_dead this)
			(def this :sp_dead :t :sp_frame -1)
			(when (defq addon (get :sp_addon this))
				(def this :sp_addon :nil)
				(. addon :sp_kill))
			(when (defq death (get :sp_death this))
				(def this :sp_death :nil)
				(death this))
			(. this :sub))
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
							(progn
								(env-tuck comp)
								((get :update comp) this comp)
								(env-tuck))
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
					(when (and (> w 0) (> h 0))
						(defq zoom (get :zoom this)
							fw (get :sp_frame_w this)
							fh (get :sp_frame_h this)
							scaled_fh (* zoom fh)
							scaled_fw (* zoom fw)
							raw_y (+ (* f scaled_fh) (* zoom (get :sp_oy this)))
							col (/ raw_y th)
							sy (% raw_y th)
							sx (* col scaled_fw)
							dir (get :sp_dir this))
						(when (and (>= dir 0) (get :sp_canvas_r this))
							(setq sx (- tw (+ sx scaled_fw))))
						(ifn (= (get :sp_h this) 48)
							(. this :ctx_blit tid +argb_white 0 0 w h sx sy)
							; banner head (top 16x16)
							(. this :ctx_blit tid +argb_white 0 0 scaled_fw scaled_fh sx sy)
							; pole 1 (middle 16x16)
							(. this :ctx_blit tid +argb_white 0 scaled_fh scaled_fw scaled_fh 0 0)
							; pole 2 (bottom 16x16)
							(. this :ctx_blit tid +argb_white 0 (* scaled_fh 2) scaled_fw scaled_fh 0 0))))))
		this)
	)