;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer Mode GUI Import
; This file wraps the standard GUI macros to track UI structure
; for visual editing in the Designer application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;import standard GUI (this creates all the widgets)
(import "gui/lisp.inc")

;global designer tracking state
(defq *designer-enabled* :t)
(defq *designer-ui-tree* :nil)
(defq *designer-ui-stack* (list))
(defq *designer-ui-counter* 0)

;designer UI element metadata structure
(defun designer-make-element (type name constructor props body)
	; (designer-make-element type name constructor props body) -> element
	(scatter (Lmap)
		:id (++ *designer-ui-counter*)
		:type type
		:name (str name)
		:constructor (str constructor)
		:props (if props (list props) (list))
		:body (if body (list body) (list))
		:children (list)))

(defun designer-push-element (element)
	; (designer-push-element element) -> element
	(when *designer-enabled*
		(cond
			;first element becomes the root
			((not *designer-ui-tree*)
				(setq *designer-ui-tree* element))
			;otherwise add to current parent
			((nempty? *designer-ui-stack*)
				(defq parent (first *designer-ui-stack*))
				(push (get :children parent) element)))
		;push to stack for potential children
		(push *designer-ui-stack* element))
	element)

(defun designer-pop-element ()
	; (designer-pop-element) -> :nil
	(when *designer-enabled*
		(pop *designer-ui-stack*))
	:nil)

(defun designer-get-tree ()
	; (designer-get-tree) -> tree
	*designer-ui-tree*)

(defun designer-reset ()
	; (designer-reset) -> :nil
	(setq *designer-ui-tree* :nil
		*designer-ui-stack* (list)
		*designer-ui-counter* 0)
	:nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wrapped UI Macros
; These execute normally but also track structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;save original macros with different names
(redefmacro ui-root-original (n c &optional p &rest x)
	; Original ui-root macro
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn (defq _ui (list (defq ,n ,c)))
				~x (setq _ui :nil)))
			(static-qq (progn (defq _ui (list (defq ,n ,c)))
				(undef ,n ~e) ~x (setq _ui :nil))))
		(if (= 0 (length e))
			(static-qq (progn (defq _ui (list (defq ,n ,c)))
				(def ,n ~p) ~x (setq _ui :nil)))
			(static-qq (progn (defq _ui (list (defq ,n ,c)))
				(def ,n ~p) (undef ,n ~e) ~x (setq _ui :nil))))))

(redefmacro ui-element-original (n c &optional p &rest x)
	; Original ui-element macro
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn (. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				~x (pop _ui)))
			(static-qq (progn (. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(undef ,n ~e) ~x (pop _ui))))
		(if (= 0 (length e))
			(static-qq (progn (. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p) ~x (pop _ui)))
			(static-qq (progn (. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p) (undef ,n ~e) ~x (pop _ui))))))

;redefine ui-root to track
(redefmacro ui-root (n c &optional p &rest x)
	; (ui-root name constructor [props] [body]) -> view
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-root" ',n ',c ',p '(~x)))
		(ui-root-original ,n ,c ,p ~x)
		(designer-pop-element)
		,n)))

;redefine ui-element to track
(redefmacro ui-element (n c &optional p &rest x)
	; (ui-element name constructor [props] [body]) -> view
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-element" ',n ',c ',p '(~x)))
		(ui-element-original ,n ,c ,p ~x)
		(designer-pop-element))))

;override higher-level macros to use tracking versions
(redefmacro ui-window (n &optional p &rest x)
	; (ui-window name [props] [body]) -> window
	(ui-props p
		:font *env_window_font*
		:ink_color *env_ink_col*
		:color *env_window_col*
		:hint_color *env_hint_col*
		:no_hint_color *env_no_hint_col*
		:border *env_window_border*
		:shadow *env_window_shadow*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-window" ',n '(Window) ',p '(~x)))
		(ui-root-original ,n (Window) ,p
			(ui-flow-original _ (:flow_flags +flow_down_fill) ~x))
		(designer-pop-element)
		,n)))

(redefmacro ui-flow (n &optional p &rest x)
	; (ui-flow name [props] [body]) -> flow
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-flow" ',n '(Flow) ',p '(~x)))
		(ui-element-original ,n (Flow) ,p ~x)
		(designer-pop-element))))

(redefmacro ui-grid (n &optional p &rest x)
	; (ui-grid name [props] [body]) -> grid
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-grid" ',n '(Grid) ',p '(~x)))
		(ui-element-original ,n (Grid) ,p ~x)
		(designer-pop-element))))

(redefmacro ui-button (n &optional p &rest x)
	; (ui-button name [props] [body]) -> button
	(ui-props p
		:flow_flags (bit-mask +flow_flag_down +flow_flag_align_hcenter +flow_flag_align_vcenter)
		:border *env_button_border*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-button" ',n '(Button) ',p '(~x)))
		(ui-element-original ,n (Button) ,p ~x)
		(designer-pop-element))))

(redefmacro ui-label (n &optional p &rest x)
	; (ui-label name [props] [body]) -> label
	(ui-props p
		:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter)
		:border *env_label_border*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-label" ',n '(Label) ',p '(~x)))
		(ui-element-original ,n (Label) ,p ~x)
		(designer-pop-element))))

(redefmacro ui-text (n &optional p)
	; (ui-text name [props]) -> text
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-text" ',n '(Text) ',p :nil))
		(ui-element-original ,n (Text) ,p)
		(designer-pop-element))))

(redefmacro ui-textfield (n &optional p)
	; (ui-textfield name [props]) -> textfield
	(ui-props p
		:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter)
		:border *env_textfield_border*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-textfield" ',n '(Textfield) ',p :nil))
		(ui-element-original ,n (Textfield) ,p)
		(designer-pop-element))))

(redefmacro ui-slider (n &optional p)
	; (ui-slider name [props]) -> slider
	(ui-props p
		:color *env_slider_col*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-slider" ',n '(Slider) ',p :nil))
		(ui-element-original ,n (Slider) ,p)
		(designer-pop-element))))

(redefmacro ui-title (n &optional p)
	; (ui-title name [props]) -> title
	(ui-props p
		:font *env_title_font*
		:border *env_title_border*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-title" ',n '(Title) ',p :nil))
		(ui-element-original ,n (Title) ,p)
		(designer-pop-element))))

(redefmacro ui-canvas (n w h s &optional p)
	; (ui-canvas name width height scale [props]) -> canvas
	(ui-props p
		:color 0)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-canvas" ',n '(Canvas ,w ,h ,s) ',p :nil))
		(ui-element-original ,n (Canvas ,w ,h ,s) ,p)
		(designer-pop-element))))

(redefmacro ui-vdu (n &optional p)
	; (ui-vdu name [props]) -> vdu
	(ui-props p
		:font *env_terminal_font*
		:color 0)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-vdu" ',n '(Vdu) ',p :nil))
		(ui-element-original ,n (Vdu) ,p)
		(designer-pop-element))))

(redefmacro ui-scroll (n f &optional p &rest x)
	; (ui-scroll name flags [props] [body]) -> scroll
	(ui-props p
		:color *env_slider_col*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-scroll" ',n '(Scroll ,f) ',p '(~x)))
		(ui-element-original ,n (Scroll ,f) ,p ~x)
		(designer-pop-element))))

(redefmacro ui-backdrop (n &optional p &rest x)
	; (ui-backdrop name [props] [body]) -> backdrop
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-backdrop" ',n '(Backdrop) ',p '(~x)))
		(ui-element-original ,n (Backdrop) ,p ~x)
		(designer-pop-element))))

(redefmacro ui-progress (n &optional p)
	; (ui-progress name [props]) -> progress
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-progress" ',n '(Progress) ',p :nil))
		(ui-element-original ,n (Progress) ,p)
		(designer-pop-element))))

(redefmacro ui-view (n &optional p &rest x)
	; (ui-view name [props] [body]) -> view
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-view" ',n '(View) ',p '(~x)))
		(ui-element-original ,n (View) ,p ~x)
		(designer-pop-element))))

(redefmacro ui-stack (n t &optional p &rest x)
	; (ui-stack name tabs [props] [body]) -> stack
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-stack" ',n '(Stack ,t) ',p '(~x)))
		(ui-element-original ,n (Stack ,t) ,p ~x)
		(designer-pop-element))))

(redefmacro ui-radio-bar (n s &optional p)
	; (ui-radio-bar name symbols [props])
	(ui-props p
		:color *env_toolbar_col*
		:font *env_toolbar_font*
		:border *env_button_border*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-radio-bar" ',n '(Radiobar '(~s)) ',p :nil))
		(ui-element-original ,n (Radiobar '(~s)) ,p)
		(designer-pop-element))))

(redefmacro ui-toggle-bar (n s &optional p)
	; (ui-toggle-bar name symbols [props])
	(ui-props p
		:color *env_toolbar_col*
		:font *env_toolbar_font*
		:border *env_button_border*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-toggle-bar" ',n '(Radiobar '(~s) :t) ',p :nil))
		(ui-element-original ,n (Radiobar '(~s) :t) ,p)
		(designer-pop-element))))

(redefmacro ui-tool-bar (n &optional p &rest x)
	; (ui-tool-bar name [props] [body]) -> flow
	(ui-props p
		:flow_flags +flow_right
		:color *env_toolbar_col* :font *env_toolbar_font*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-tool-bar" ',n '(Flow) ',p '(~x)))
		(ui-flow-original ,n ,p ~x)
		(designer-pop-element))))

(redefmacro ui-title-bar (n s b e &optional p)
	; (ui-title-bar name title symbols event [props]) -> flow
	(ui-props p
		:flow_flags +flow_left_fill
		:color *env_title_col*
		:border *env_title_buttons_border*
		:font *env_title_buttons_font*)
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-title-bar" ',n '(Flow) ',p '()))
		(ui-flow-original _ ,p
			(ui-buttons ,b ,e)
			(ui-title ,n (:text ,s)))
		(designer-pop-element))))

(redefmacro ui-tree (n e &optional p)
	; (ui-tree name event [props]) -> tree
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-tree" ',n '(Tree ,e) ',p :nil))
		(ui-element-original ,n (Tree ,e) ,p)
		(designer-pop-element))))

(redefmacro ui-files (n t e &optional p)
	; (ui-files name title event [props]) -> files
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-files" ',n '(Files ,t ,e) ',p :nil))
		(ui-element-original ,n (Files ,t ,e) ,p)
		(designer-pop-element))))

(redefmacro ui-spinner (n &optional p)
	; (ui-spinner name [props]) -> spinner
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-spinner" ',n '(Spinner) ',p :nil))
		(ui-element-original ,n (Spinner) ,p)
		(designer-pop-element))))

(redefmacro ui-hchart (n t m &optional p)
	; (ui-hchart name title num_marks [props]) -> hchart
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-hchart" ',n '(Hchart ,t ,m) ',p :nil))
		(ui-element-original ,n (Hchart ,t ,m) ,p)
		(designer-pop-element))))

(redefmacro ui-stroke (n &optional p)
	; (ui-stroke name [props]) -> stroke
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-stroke" ',n '(Stroke) ',p :nil))
		(ui-element-original ,n (Stroke) ,p)
		(designer-pop-element))))
