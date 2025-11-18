;; Canvas 2D Context API
;; Implements CanvasRenderingContext2D interface

(defclass canvas-2d-context nil
	(defq
		canvas nil
		width 300
		height 150
		; Drawing state
		fill_style "#000000"
		stroke_style "#000000"
		line_width 1
		font "10px sans-serif"
		; Path state
		current_path (list)
		path_begun :nil
		; Transform state
		transform_matrix (list 1 0 0 1 0 0)  ; [a b c d e f] identity matrix
		; State stack
		state_stack (list)
		; Drawing commands (for later rendering)
		commands (list)))

(defmethod :init canvas-2d-context (canvas-elem)
	(setq this-canvas canvas-elem
		this-fill_style "#000000"
		this-stroke_style "#000000"
		this-line_width 1
		this-font "10px sans-serif"
		this-current_path (list)
		this-path_begun :nil
		this-transform_matrix (list 1 0 0 1 0 0)
		this-state_stack (list)
		this-commands (list))

	; Get dimensions from canvas element
	(when canvas-elem
		(defq w-attr (. canvas-elem :get-attribute "width"))
		(defq h-attr (. canvas-elem :get-attribute "height"))
		(when w-attr (setq this-width (str-to-num w-attr)))
		(when h-attr (setq this-height (str-to-num h-attr))))

	this)

;; Style properties

(defmethod :set-fill-style canvas-2d-context (style)
	; Set fill color/style
	; ctx.fillStyle = "#FF0000"
	(setq this-fill_style style)
	:nil)

(defmethod :get-fill-style canvas-2d-context ()
	; Get fill style
	this-fill_style)

(defmethod :set-stroke-style canvas-2d-context (style)
	; Set stroke color/style
	; ctx.strokeStyle = "#00FF00"
	(setq this-stroke_style style)
	:nil)

(defmethod :get-stroke-style canvas-2d-context ()
	; Get stroke style
	this-stroke_style)

(defmethod :set-line-width canvas-2d-context (width)
	; Set line width
	; ctx.lineWidth = 5
	(setq this-line_width width)
	:nil)

(defmethod :get-line-width canvas-2d-context ()
	; Get line width
	this-line_width)

(defmethod :set-font canvas-2d-context (font)
	; Set font
	; ctx.font = "24px Arial"
	(setq this-font font)
	:nil)

(defmethod :get-font canvas-2d-context ()
	; Get font
	this-font)

;; Rectangle drawing

(defmethod :fillRect canvas-2d-context (x y width height)
	; Draw filled rectangle
	; ctx.fillRect(10, 10, 50, 50)
	(defq cmd (env))
	(set-insert cmd :type :fillRect)
	(set-insert cmd :x x)
	(set-insert cmd :y y)
	(set-insert cmd :width width)
	(set-insert cmd :height height)
	(set-insert cmd :fill_style this-fill_style)
	(set-insert cmd :transform (copy-list this-transform_matrix))
	(push this-commands cmd)
	:nil)

(defmethod :strokeRect canvas-2d-context (x y width height)
	; Draw rectangle outline
	; ctx.strokeRect(10, 10, 50, 50)
	(defq cmd (env))
	(set-insert cmd :type :strokeRect)
	(set-insert cmd :x x)
	(set-insert cmd :y y)
	(set-insert cmd :width width)
	(set-insert cmd :height height)
	(set-insert cmd :stroke_style this-stroke_style)
	(set-insert cmd :line_width this-line_width)
	(set-insert cmd :transform (copy-list this-transform_matrix))
	(push this-commands cmd)
	:nil)

(defmethod :clearRect canvas-2d-context (x y width height)
	; Clear rectangle area
	; ctx.clearRect(10, 10, 50, 50)
	(defq cmd (env))
	(set-insert cmd :type :clearRect)
	(set-insert cmd :x x)
	(set-insert cmd :y y)
	(set-insert cmd :width width)
	(set-insert cmd :height height)
	(set-insert cmd :transform (copy-list this-transform_matrix))
	(push this-commands cmd)
	:nil)

;; Path methods

(defmethod :beginPath canvas-2d-context ()
	; Begin new path
	; ctx.beginPath()
	(setq this-current_path (list))
	(setq this-path_begun :t)
	:nil)

(defmethod :moveTo canvas-2d-context (x y)
	; Move to point without drawing
	; ctx.moveTo(10, 10)
	(defq point (env))
	(set-insert point :type :moveTo)
	(set-insert point :x x)
	(set-insert point :y y)
	(push this-current_path point)
	:nil)

(defmethod :lineTo canvas-2d-context (x y)
	; Draw line to point
	; ctx.lineTo(100, 100)
	(defq point (env))
	(set-insert point :type :lineTo)
	(set-insert point :x x)
	(set-insert point :y y)
	(push this-current_path point)
	:nil)

(defmethod :closePath canvas-2d-context ()
	; Close current path
	; ctx.closePath()
	(defq point (env))
	(set-insert point :type :closePath)
	(push this-current_path point)
	:nil)

(defmethod :arc canvas-2d-context (x y radius start-angle end-angle &optional anticlockwise)
	; Draw arc/circle
	; ctx.arc(100, 100, 50, 0, 2*Math.PI)
	(defq point (env))
	(set-insert point :type :arc)
	(set-insert point :x x)
	(set-insert point :y y)
	(set-insert point :radius radius)
	(set-insert point :start_angle start-angle)
	(set-insert point :end_angle end-angle)
	(set-insert point :anticlockwise (if anticlockwise anticlockwise :nil))
	(push this-current_path point)
	:nil)

(defmethod :stroke canvas-2d-context ()
	; Stroke current path
	; ctx.stroke()
	(when this-path_begun
		(defq cmd (env))
		(set-insert cmd :type :stroke)
		(set-insert cmd :path (copy-list this-current_path))
		(set-insert cmd :stroke_style this-stroke_style)
		(set-insert cmd :line_width this-line_width)
		(set-insert cmd :transform (copy-list this-transform_matrix))
		(push this-commands cmd))
	:nil)

(defmethod :fill canvas-2d-context ()
	; Fill current path
	; ctx.fill()
	(when this-path_begun
		(defq cmd (env))
		(set-insert cmd :type :fill)
		(set-insert cmd :path (copy-list this-current_path))
		(set-insert cmd :fill_style this-fill_style)
		(set-insert cmd :transform (copy-list this-transform_matrix))
		(push this-commands cmd))
	:nil)

;; Text methods

(defmethod :fillText canvas-2d-context (text x y &optional max-width)
	; Draw filled text
	; ctx.fillText("Hello", 10, 50)
	(defq cmd (env))
	(set-insert cmd :type :fillText)
	(set-insert cmd :text text)
	(set-insert cmd :x x)
	(set-insert cmd :y y)
	(set-insert cmd :max_width (if max-width max-width :nil))
	(set-insert cmd :fill_style this-fill_style)
	(set-insert cmd :font this-font)
	(set-insert cmd :transform (copy-list this-transform_matrix))
	(push this-commands cmd)
	:nil)

(defmethod :strokeText canvas-2d-context (text x y &optional max-width)
	; Draw outlined text
	; ctx.strokeText("Hello", 10, 50)
	(defq cmd (env))
	(set-insert cmd :type :strokeText)
	(set-insert cmd :text text)
	(set-insert cmd :x x)
	(set-insert cmd :y y)
	(set-insert cmd :max_width (if max-width max-width :nil))
	(set-insert cmd :stroke_style this-stroke_style)
	(set-insert cmd :line_width this-line_width)
	(set-insert cmd :font this-font)
	(set-insert cmd :transform (copy-list this-transform_matrix))
	(push this-commands cmd)
	:nil)

;; Transform methods

(defmethod :translate canvas-2d-context (x y)
	; Translate coordinate system
	; ctx.translate(50, 50)
	(defq m this-transform_matrix)
	(defq a (elem 0 m))
	(defq b (elem 1 m))
	(defq c (elem 2 m))
	(defq d (elem 3 m))
	(defq e (elem 4 m))
	(defq f (elem 5 m))

	(setq this-transform_matrix
		(list a b c d (+ e (* a x) (* c y)) (+ f (* b x) (* d y))))
	:nil)

(defmethod :rotate canvas-2d-context (angle)
	; Rotate coordinate system
	; ctx.rotate(Math.PI / 4)
	(defq cos-a (cos angle))
	(defq sin-a (sin angle))

	(defq m this-transform_matrix)
	(defq a (elem 0 m))
	(defq b (elem 1 m))
	(defq c (elem 2 m))
	(defq d (elem 3 m))
	(defq e (elem 4 m))
	(defq f (elem 5 m))

	(setq this-transform_matrix
		(list
			(- (* a cos-a) (* b sin-a))
			(+ (* a sin-a) (* b cos-a))
			(- (* c cos-a) (* d sin-a))
			(+ (* c sin-a) (* d cos-a))
			e
			f))
	:nil)

(defmethod :scale canvas-2d-context (x y)
	; Scale coordinate system
	; ctx.scale(2, 2)
	(defq m this-transform_matrix)
	(setq this-transform_matrix
		(list
			(* (elem 0 m) x)
			(* (elem 1 m) x)
			(* (elem 2 m) y)
			(* (elem 3 m) y)
			(elem 4 m)
			(elem 5 m)))
	:nil)

(defmethod :setTransform canvas-2d-context (a b c d e f)
	; Set transform matrix
	; ctx.setTransform(1, 0, 0, 1, 0, 0)
	(setq this-transform_matrix (list a b c d e f))
	:nil)

(defmethod :resetTransform canvas-2d-context ()
	; Reset to identity matrix
	; ctx.resetTransform()
	(setq this-transform_matrix (list 1 0 0 1 0 0))
	:nil)

;; State methods

(defmethod :save canvas-2d-context ()
	; Save current state
	; ctx.save()
	(defq state (env))
	(set-insert state :fill_style this-fill_style)
	(set-insert state :stroke_style this-stroke_style)
	(set-insert state :line_width this-line_width)
	(set-insert state :font this-font)
	(set-insert state :transform (copy-list this-transform_matrix))
	(push this-state_stack state)
	:nil)

(defmethod :restore canvas-2d-context ()
	; Restore previous state
	; ctx.restore()
	(when (> (length this-state_stack) 0)
		(defq state (pop this-state_stack))
		(setq this-fill_style (get state :fill_style))
		(setq this-stroke_style (get state :stroke_style))
		(setq this-line_width (get state :line_width))
		(setq this-font (get state :font))
		(setq this-transform_matrix (get state :transform)))
	:nil)

;; Helper methods

(defmethod :get-commands canvas-2d-context ()
	; Get list of drawing commands (for rendering)
	this-commands)

(defmethod :clear-commands canvas-2d-context ()
	; Clear drawing commands
	(setq this-commands (list))
	:nil)

;; Helper functions

(defun copy-list (lst)
	; Deep copy a list
	(defq result (list))
	(each! 0 -1 (lambda (item) (push result item)) lst)
	result)

(defun str-to-num (str)
	; Convert string to number
	(when str
		(catch
			(read str)
			(lambda (e) 0))))

(defun cos (angle)
	; Cosine function (placeholder - would use real math lib)
	; For now, approximate
	1.0)

(defun sin (angle)
	; Sine function (placeholder - would use real math lib)
	; For now, approximate
	0.0)

