;import symbols
(import "apps/login/pupa.inc")
(import "lib/consts/colors.inc")

;lisp bindings
(ffi path "class/array/lisp_path" 0)
(import "gui/view/lisp.inc")
(import "gui/window/lisp.inc")
(import "gui/backdrop/lisp.inc")
(import "gui/slider/lisp.inc")
(import "gui/progress/lisp.inc")
(import "gui/scroll/lisp.inc")
(import "gui/textfield/lisp.inc")
(import "gui/text/lisp.inc")
(import "gui/title/lisp.inc")
(import "gui/grid/lisp.inc")
(import "gui/flow/lisp.inc")
(import "gui/tree/lisp.inc")
(import "gui/vdu/lisp.inc")
(import "gui/button/lisp.inc")
(import "gui/label/lisp.inc")
(import "gui/path/lisp.inc")
(import "gui/canvas/lisp.inc")
(import "gui/texture/lisp.inc")
(import "gui/vdu/lisp.inc")
(import "gui/font/lisp.inc")
(import "gui/pixmap/lisp.inc")
(import "gui/edit/lisp.inc")
(import "gui/spinner/lisp.inc")

;ui tree helper macros
(defun ui-merge-props (p)
	; (ui-merge-props props) -> props
	(defq ps (clear '()) pv (clear '()))
	(each! 0 -1 (lambda (s v)
		(cond
			((defq i (find-rev s ps))
				(elem-set i ps s) (elem-set i pv v))
			(t  (push ps s) (push pv v))))
		(unzip (opt p '()) (list (list) (list))))
	(zip ps pv))

(defmacro ui-props (p &rest x)
	; (ui-props props [props]) -> props
	`(setq ,p (cat ',x (opt ,p '()))))

(defmacro ui-root (n c &optional p &rest x)
	; (ui-root name func [props] [body]) -> view
	(setq p (ui-merge-props p))
	(if (/= 0 (length p))
		`(progn (defq _ui (list (defq ,n ,c))) (def ,n ~p) ~x (setq _ui nil))
		`(progn (defq _ui (list (defq ,n ,c))) ~x (setq _ui nil))))

(defmacro ui-element (n c &optional p &rest x)
	; (ui-element name func [props] [body]) -> view
	(setq p (ui-merge-props p))
	(if (/= 0 (length p))
		`(progn (. (elem-get -3 (push _ui (defq ,n ,c))) :add_child ,n) (def ,n ~p) ~x (pop _ui))
		`(progn (. (elem-get -3 (push _ui (defq ,n ,c))) :add_child ,n) ~x (pop _ui))))

(defmacro ui-window (n &optional p &rest x)
	; (ui-window name [props] [body]) -> window
	(ui-props p
		:font *env_window_font*
		:ink_color *env_ink_col*
		:color *env_window_col*
		:hint_color *env_hint_col*
		:no_hint_color *env_no_hint_col*
		:border *env_window_border*
		:shadow *env_window_shadow*)
	`(ui-root ,n (Window) ,p
		(ui-flow _ (:flow_flags +flow_down_fill) ~x)))

(defmacro ui-flow (n &optional p &rest x)
	; (ui-flow name [props] [body]) -> flow
	`(ui-element ,n (Flow) ,p ~x))

(defmacro ui-grid (n &optional p &rest x)
	; (ui-grid name [props] [body]) -> grid
	`(ui-element ,n (Grid) ,p ~x))

(defmacro ui-tree (n e &optional p)
	; (ui-tree name event [props]) -> tree
	`(ui-element ,n (Tree ,e) ,p))

(defmacro ui-spinner (n &optional p)
	; (ui-spinner name [props]) -> spinner
	`(ui-element ,n (Spinner) ,p))

(defmacro ui-title (n &optional p)
	; (ui-title name [props]) -> title
	(ui-props p
		:font *env_title_font*
		:border *env_title_border*)
	`(ui-element ,n (Title) ,p))

(defmacro ui-title-bar (n s b e &optional p)
	; (ui-title-bar name title symbols event [props]) -> flow
	(ui-props p
		:flow_flags +flow_left_fill
		:color *env_title_col*
		:font *env_title_buttons_font*)
	`(ui-flow _ ,p
		(ui-buttons ,b ,e (:border *env_title_buttons_border*))
		(ui-title ,n (:text ,s))))

(defmacro ui-label (n &optional p &rest x)
	; (ui-label name [props] [body]) -> label
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_right +flow_flag_align_vcenter))
		:border *env_label_border*)
	`(ui-element ,n (Label) ,p ~x))

(defmacro ui-button (n &optional p &rest x)
	; (ui-button name [props] [body]) -> button
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_down +flow_flag_align_hcenter +flow_flag_align_vcenter))
		:border *env_button_border*)
	`(ui-element ,n (Button) ,p ~x))

(defmacro ui-buttons (s e &optional p)
	; (ui-buttons symbols event [props])
	(setq s (map (lambda (_) (if (num? _) (num-to-utf8 _) _)) s))
	(ui-props p
		:text _s)
	`(each (lambda (_s) (. (ui-button __ ,p) :connect (+ _ ,e))) '(~s)))

(defmacro ui-tool-bar (n &optional p &rest x)
	; (ui-tool-bar name [props] [body]) -> flow
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_right +flow_flag_fillh))
		:color *env_toolbar_col* :font *env_toolbar_font*)
	`(ui-flow ,n ,p ~x))

(defmacro ui-textfield (n &optional p)
	; (ui-textfield name [props]) -> textfield
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_right +flow_flag_align_vcenter))
		:border *env_textfield_border*)
	`(ui-element ,n (Textfield) ,p))

(defmacro ui-slider (n &optional p)
	; (ui-slider name [props]) -> slider
	(ui-props p
		:color *env_slider_col*)
	`(ui-element ,n (Slider) ,p))

(defmacro ui-scroll (n f &optional p &rest x)
	; (ui-scroll name flags [props] [body]) -> scroll
	(ui-props p
		:color *env_slider_col*)
	`(ui-element ,n (Scroll ,f) ,p ~x))

(defmacro ui-backdrop (n &optional p &rest x)
	; (ui-backdrop name [props] [body]) -> backdrop
	(ui-props p
		:color *env_backdrop_col*
		:ink_color *env_backdrop_ink_col*)
	`(ui-element ,n (Backdrop) ,p ~x))

(defmacro ui-progress (n &optional p)
	; (ui-progress name [props]) -> progress
	`(ui-element ,n (Progress) ,p))

(defmacro ui-canvas (n w h s &optional p)
	; (ui-canvas name width height scale [props]) -> canvas
	(ui-props p
		:color 0)
	`(ui-element ,n (Canvas ,w ,h ,s) ,p))

(defmacro ui-vdu (n &optional p)
	; (ui-vdu name [props]) -> vdu
	(ui-props p
		:font *env_terminal_font*
		:color 0)
	`(ui-element ,n (Vdu) ,p))

(defmacro ui-view (n &optional p &rest x)
	; (ui-view name [props] [body]) -> view
	`(ui-element ,n (View) ,p ~x))
