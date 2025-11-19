;; Canvas 2D Context API
;; Implements CanvasRenderingContext2D interface

(import "lib/class/class.inc")

(defclass canvas-2d-context () :nil
	; Canvas 2D rendering context
	(def this
		:canvas :nil
		:width 300
		:height 150
		; Drawing state
		:fill_style "#000000"
		:stroke_style "#000000"
		:line_width 1
		:font "10px sans-serif"
		; Path state
		:current_path (list)
		:path_begun :nil
		; Transform state
		:transform_matrix (list 1 0 0 1 0 0)  ; [a b c d e f] identity matrix
		; State stack
		:state_stack (list)
		; Drawing commands (for later rendering)
		:commands (list))

	(defmethod :init (canvas-elem)
		(def this
			:canvas canvas-elem
			:fill_style "#000000"
			:stroke_style "#000000"
			:line_width 1
			:font "10px sans-serif"
			:current_path (list)
			:path_begun :nil
			:transform_matrix (list 1 0 0 1 0 0)
			:state_stack (list)
			:commands (list))

		; Get dimensions from canvas element
		(when canvas-elem
			(defq w-attr (. canvas-elem :get-attribute "width"))
			(defq h-attr (. canvas-elem :get-attribute "height"))
			(when w-attr (def this :width (str-to-num w-attr)))
			(when h-attr (def this :height (str-to-num h-attr))))

		this)

	;; Style properties

	(defmethod :set-fill-style (style)
		; Set fill color/style
		; ctx.fillStyle = "#FF0000"
		(def this :fill_style style)
		:nil)

	(defmethod :get-fill-style ()
		; Get fill style
		(get :fill_style this))

	(defmethod :set-stroke-style (style)
		; Set stroke color/style
		; ctx.strokeStyle = "#00FF00"
		(def this :stroke_style style)
		:nil)

	(defmethod :get-stroke-style ()
		; Get stroke style
		(get :stroke_style this))

	(defmethod :set-line-width (width)
		; Set line width
		; ctx.lineWidth = 5
		(def this :line_width width)
		:nil)

	(defmethod :get-line-width ()
		; Get line width
		(get :line_width this))

	(defmethod :set-font (font)
		; Set font
		; ctx.font = "24px Arial"
		(def this :font font)
		:nil)

	(defmethod :get-font ()
		; Get font
		(get :font this))

	;; Rectangle drawing

	(defmethod :fillRect (x y width height)
		; Draw filled rectangle
		; ctx.fillRect(10, 10, 50, 50)
		(defq cmd (env))
		(set cmd :type :fillRect)
		(set cmd :x x)
		(set cmd :y y)
		(set cmd :width width)
		(set cmd :height height)
		(set cmd :fill_style (get :fill_style this))
		(set cmd :transform (copy-list (get :transform_matrix this)))
		(defq cmds (get :commands this))
		(push cmds cmd)
		(def this :commands cmds)
		:nil)

	(defmethod :strokeRect (x y width height)
		; Draw rectangle outline
		; ctx.strokeRect(10, 10, 50, 50)
		(defq cmd (env))
		(set cmd :type :strokeRect)
		(set cmd :x x)
		(set cmd :y y)
		(set cmd :width width)
		(set cmd :height height)
		(set cmd :stroke_style (get :stroke_style this))
		(set cmd :line_width (get :line_width this))
		(set cmd :transform (copy-list (get :transform_matrix this)))
		(defq cmds (get :commands this))
		(push cmds cmd)
		(def this :commands cmds)
		:nil)

	(defmethod :clearRect (x y width height)
		; Clear rectangle area
		; ctx.clearRect(10, 10, 50, 50)
		(defq cmd (env))
		(set cmd :type :clearRect)
		(set cmd :x x)
		(set cmd :y y)
		(set cmd :width width)
		(set cmd :height height)
		(set cmd :transform (copy-list (get :transform_matrix this)))
		(defq cmds (get :commands this))
		(push cmds cmd)
		(def this :commands cmds)
		:nil)

	;; Path methods

	(defmethod :beginPath ()
		; Begin new path
		; ctx.beginPath()
		(def this :current_path (list))
		(def this :path_begun :t)
		:nil)

	(defmethod :moveTo (x y)
		; Move to point without drawing
		; ctx.moveTo(10, 10)
		(defq point (env))
		(set point :type :moveTo)
		(set point :x x)
		(set point :y y)
		(defq path (get :current_path this))
		(push path point)
		(def this :current_path path)
		:nil)

	(defmethod :lineTo (x y)
		; Draw line to point
		; ctx.lineTo(100, 100)
		(defq point (env))
		(set point :type :lineTo)
		(set point :x x)
		(set point :y y)
		(defq path (get :current_path this))
		(push path point)
		(def this :current_path path)
		:nil)

	(defmethod :closePath ()
		; Close current path
		; ctx.closePath()
		(defq point (env))
		(set point :type :closePath)
		(defq path (get :current_path this))
		(push path point)
		(def this :current_path path)
		:nil)

	(defmethod :arc (x y radius start-angle end-angle &optional anticlockwise)
		; Draw arc/circle
		; ctx.arc(100, 100, 50, 0, 2*Math.PI)
		(defq point (env))
		(set point :type :arc)
		(set point :x x)
		(set point :y y)
		(set point :radius radius)
		(set point :start_angle start-angle)
		(set point :end_angle end-angle)
		(set point :anticlockwise (if anticlockwise anticlockwise :nil))
		(defq path (get :current_path this))
		(push path point)
		(def this :current_path path)
		:nil)

	(defmethod :stroke ()
		; Stroke current path
		; ctx.stroke()
		(when (get :path_begun this)
			(defq cmd (env))
			(set cmd :type :stroke)
			(set cmd :path (copy-list (get :current_path this)))
			(set cmd :stroke_style (get :stroke_style this))
			(set cmd :line_width (get :line_width this))
			(set cmd :transform (copy-list (get :transform_matrix this)))
			(defq cmds (get :commands this))
			(push cmds cmd)
			(def this :commands cmds))
		:nil)

	(defmethod :fill ()
		; Fill current path
		; ctx.fill()
		(when (get :path_begun this)
			(defq cmd (env))
			(set cmd :type :fill)
			(set cmd :path (copy-list (get :current_path this)))
			(set cmd :fill_style (get :fill_style this))
			(set cmd :transform (copy-list (get :transform_matrix this)))
			(defq cmds (get :commands this))
			(push cmds cmd)
			(def this :commands cmds))
		:nil)

	;; Text methods

	(defmethod :fillText (text x y &optional max-width)
		; Draw filled text
		; ctx.fillText("Hello", 10, 50)
		(defq cmd (env))
		(set cmd :type :fillText)
		(set cmd :text text)
		(set cmd :x x)
		(set cmd :y y)
		(set cmd :max_width (if max-width max-width :nil))
		(set cmd :fill_style (get :fill_style this))
		(set cmd :font (get :font this))
		(set cmd :transform (copy-list (get :transform_matrix this)))
		(defq cmds (get :commands this))
		(push cmds cmd)
		(def this :commands cmds)
		:nil)

	(defmethod :strokeText (text x y &optional max-width)
		; Draw outlined text
		; ctx.strokeText("Hello", 10, 50)
		(defq cmd (env))
		(set cmd :type :strokeText)
		(set cmd :text text)
		(set cmd :x x)
		(set cmd :y y)
		(set cmd :max_width (if max-width max-width :nil))
		(set cmd :stroke_style (get :stroke_style this))
		(set cmd :line_width (get :line_width this))
		(set cmd :font (get :font this))
		(set cmd :transform (copy-list (get :transform_matrix this)))
		(defq cmds (get :commands this))
		(push cmds cmd)
		(def this :commands cmds)
		:nil)

	;; Transform methods

	(defmethod :translate (x y)
		; Translate coordinate system
		; ctx.translate(50, 50)
		(defq m (get :transform_matrix this))
		(defq a (elem 0 m))
		(defq b (elem 1 m))
		(defq c (elem 2 m))
		(defq d (elem 3 m))
		(defq e (elem 4 m))
		(defq f (elem 5 m))

		(def this :transform_matrix
			(list a b c d (+ e (* a x) (* c y)) (+ f (* b x) (* d y))))
		:nil)

	(defmethod :rotate (angle)
		; Rotate coordinate system
		; ctx.rotate(Math.PI / 4)
		(defq cos-a (cos angle))
		(defq sin-a (sin angle))

		(defq m (get :transform_matrix this))
		(defq a (elem 0 m))
		(defq b (elem 1 m))
		(defq c (elem 2 m))
		(defq d (elem 3 m))
		(defq e (elem 4 m))
		(defq f (elem 5 m))

		(def this :transform_matrix
			(list
				(- (* a cos-a) (* b sin-a))
				(+ (* a sin-a) (* b cos-a))
				(- (* c cos-a) (* d sin-a))
				(+ (* c sin-a) (* d cos-a))
				e
				f))
		:nil)

	(defmethod :scale (x y)
		; Scale coordinate system
		; ctx.scale(2, 2)
		(defq m (get :transform_matrix this))
		(def this :transform_matrix
			(list
				(* (elem 0 m) x)
				(* (elem 1 m) x)
				(* (elem 2 m) y)
				(* (elem 3 m) y)
				(elem 4 m)
				(elem 5 m)))
		:nil)

	(defmethod :setTransform (a b c d e f)
		; Set transform matrix
		; ctx.setTransform(1, 0, 0, 1, 0, 0)
		(def this :transform_matrix (list a b c d e f))
		:nil)

	(defmethod :resetTransform ()
		; Reset to identity matrix
		; ctx.resetTransform()
		(def this :transform_matrix (list 1 0 0 1 0 0))
		:nil)

	;; State methods

	(defmethod :save ()
		; Save current state
		; ctx.save()
		(defq state (env))
		(set state :fill_style (get :fill_style this))
		(set state :stroke_style (get :stroke_style this))
		(set state :line_width (get :line_width this))
		(set state :font (get :font this))
		(set state :transform (copy-list (get :transform_matrix this)))
		(defq stk (get :state_stack this))
		(push stk state)
		(def this :state_stack stk)
		:nil)

	(defmethod :restore ()
		; Restore previous state
		; ctx.restore()
		(defq stk (get :state_stack this))
		(when (> (length stk) 0)
			(defq state (pop stk))
			(def this :state_stack stk)
			(def this :fill_style (get state :fill_style))
			(def this :stroke_style (get state :stroke_style))
			(def this :line_width (get state :line_width))
			(def this :font (get state :font))
			(def this :transform_matrix (get state :transform)))
		:nil)

	;; Helper methods

	(defmethod :get-commands ()
		; Get list of drawing commands (for rendering)
		(get :commands this))

	(defmethod :clear-commands ()
		; Clear drawing commands
		(def this :commands (list))
		:nil))

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
