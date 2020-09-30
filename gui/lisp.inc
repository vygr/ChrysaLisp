;import symbols
(import "sys/symbols.inc")
(import "apps/login/pupa.inc")

(defun-bind ui-merge-props (p)
	;(ui-merge-props props) -> props
	(defq ps (list) pv (list))
	(each! 0 -1 (lambda (s v)
		(cond
			((defq i (find-rev s ps))
				(elem-set i ps s) (elem-set i pv v))
			(t	(push ps s) (push pv v))))
		(reduce (lambda (l e)
			(push (elem (logand _ 1) l) e) l) (opt p '()) (list (list) (list))))
	(setq p (list))
	(each (lambda (s v) (push p (list 'quote s) v)) ps pv) p)

(defmacro-bind ui-props (p &rest x)
	;(ui-props props [props]) -> props
	`(setq ,p (cat ',x (opt ,p '()))))

(defmacro ui-tree (n c &optional p &rest x)
	;(ui-tree name func [props] [body]) -> view
	(setq p (ui-merge-props p))
	(if (/= 0 (length p))
		`(progn (defq _ui (list (defq ,n ,c))) (def ,n ~p) ~x (setq _ui nil))
		`(progn (defq _ui (list (defq ,n ,c))) ~x (setq _ui nil))))

(defmacro ui-element (n c &optional p &rest x)
	;(ui-element name func [props] [body]) -> view
	(setq p (ui-merge-props p))
	(if (/= 0 (length p))
		`(progn (view-add-child (elem -3 (push _ui (defq ,n ,c))) ,n) (def ,n ~p) ~x (pop _ui))
		`(progn (view-add-child (elem -3 (push _ui (defq ,n ,c))) ,n) ~x (pop _ui))))

(defmacro ui-window (n &optional p &rest x)
	;(ui-window name [props] [body]) -> window
	(ui-props p :font *env_window_font* :ink_color *env_ink_col* :color *env_window_col*
		:border *env_window_border*)
	`(ui-tree ,n (create-window) ,p
		(ui-flow _ (:flow_flags flow_down_fill) ~x)))

(defmacro ui-flow (n &optional p &rest x)
	;(ui-flow name [props] [body]) -> flow
	`(ui-element ,n (create-flow) ,p ~x))

(defmacro ui-grid (n &optional p &rest x)
	;(ui-grid name [props] [body]) -> grid
	`(ui-element ,n (create-grid) ,p ~x))

(defmacro ui-title (n &optional p)
	;(ui-title name [props]) -> title
	(ui-props p :font *env_title_font* :border *env_title_border*)
	`(ui-element ,n (create-title) ,p))

(defmacro ui-title-bar (n s b e &optional p)
	;(ui-title-bar name title symbols events [props]) -> flow
	(ui-props p :flow_flags flow_left_fill :color *env_title_col* :font *env_title_buttons_font*)
	`(ui-flow _ ,p
		(ui-buttons ,b ,e (:border *env_title_buttons_border*))
		(ui-title ,n (:text ,s))))

(defmacro ui-label (n &optional p &rest x)
	;(ui-label name [props] [body]) -> label
	(ui-props p :flow_flags (logior flow_flag_right flow_flag_align_vcenter) :border *env_label_border*)
	`(ui-element ,n (create-label) ,p ~x))

(defmacro ui-button (n &optional p &rest x)
	;(ui-button name [props] [body]) -> button
	(ui-props p :flow_flags (logior flow_flag_down flow_flag_align_hcenter flow_flag_align_vcenter)
		:border *env_button_border*)
	`(ui-element ,n (create-button) ,p ~x))

(defmacro ui-buttons (s e &optional p l)
	;(ui-buttons symbols events [props] [group])
	(setq s (map (lambda (_) (if (num? _) (num-to-utf8 _) _)) s))
	(ui-props p :text _s)
	(if l
		`(each (lambda (_s) (push ,l (component-connect (ui-button __ ,p) (+ _ ,e)))) '(~s))
		`(each (lambda (_s) (component-connect (ui-button __ ,p) (+ _ ,e))) '(~s))))

(defmacro ui-tool-bar (n &optional p &rest x)
	;(ui-tool-bar name [props] [body]) -> flow
	(ui-props p :flow_flags (logior flow_flag_right flow_flag_fillh) :color *env_toolbar_col*
		:font *env_toolbar_font*)
	`(ui-flow ,n ,p ~x))

(defmacro ui-textfield (n &optional p)
	;(ui-textfield name [props]) -> textfield
	(ui-props p :flow_flags (logior flow_flag_right flow_flag_align_vcenter) :border *env_textfield_border*)
	`(ui-element ,n (create-textfield) ,p))

(defmacro ui-slider (n &optional p)
	;(ui-slider name [props]) -> slider
	(ui-props p :color *env_slider_col*)
	`(ui-element ,n (create-slider) ,p))

(defmacro ui-scroll (n f &optional p &rest x)
	;(ui-scroll name [props] [body]) -> scroll
	(ui-props p :color *env_slider_col*)
	`(ui-element ,n (create-scroll ,f) ,p ~x))

(defmacro ui-backdrop (n &optional p &rest x)
	;(ui-backdrop name [props] [body]) -> backdrop
	(ui-props p :color *env_backdrop_col* :ink_color *env_backdrop_ink_col*)
	`(ui-element ,n (create-backdrop) ,p ~x))

(defmacro ui-progress (n &optional p)
	;(ui-progress name [props]) -> progress
	`(ui-element ,n (create-progress) ,p))

(defmacro ui-canvas (n w h s)
	;(ui-canvas name width height scale) -> canvas
	`(ui-element ,n (create-canvas ,w ,h ,s)))

(defmacro ui-vdu (n &optional p)
	;(ui-vdu name [props]) -> vdu
	(ui-props p :font *env_terminal_font*)
	`(ui-element ,n (create-vdu) ,p))

(defmacro ui-view (n &optional p &rest x)
	;(ui-view name [props] [body]) -> view
	`(ui-element ,n (create-view) ,p ~x))

(defmacro view-dirty-all (_)
	;(view-dirty-all view) -> view
	`(view-set-flags ,_ view_flag_dirty_all view_flag_dirty_all))

;useful flow combos
(defq flow_down_fill (logior flow_flag_down flow_flag_fillw flow_flag_lasth)
	flow_right_fill (logior flow_flag_right flow_flag_fillh flow_flag_lastw)
	flow_left_fill (logior flow_flag_left flow_flag_fillh flow_flag_lastw))

(defq scroll_flag_both (logior scroll_flag_vertical scroll_flag_horizontal))

;lisp bindings
(ffi path "class/array/lisp_path" 0)
(ffi component-connect "gui/component/lisp_connect" 0)
(ffi create-window "gui/window/lisp_create" 0)
(ffi create-backdrop "gui/backdrop/lisp_create" 0)
(ffi create-button "gui/button/lisp_create" 0)
(ffi create-flow "gui/flow/lisp_create" 0)
(ffi create-grid "gui/grid/lisp_create" 0)
(ffi create-label "gui/label/lisp_create" 0)
(ffi create-title "gui/title/lisp_create" 0)
(ffi create-textfield "gui/textfield/lisp_create" 0)
(ffi create-progress "gui/progress/lisp_create" 0)
(ffi create-scroll "gui/scroll/lisp_create" 0)
(ffi create-slider "gui/slider/lisp_create" 0)
(ffi create-vdu "gui/vdu/lisp_create" 0)
(ffi create-view "gui/view/lisp_create" 0)
(ffi gui-add "gui/gui/lisp_add" 0)
(ffi gui-add-back "gui/gui/lisp_add_back" 0)
(ffi gui-info "gui/gui/lisp_info" 0)
(ffi create-font "gui/font/lisp_create" 0)
(ffi font-glyph-paths "gui/font/lisp_glyph_paths" 0)
(ffi font-glyph-ranges "gui/font/lisp_glyph_ranges" 0)
(import "gui/component/lisp.inc")
(import "gui/view/lisp.inc")
(import "gui/path/lisp.inc")
(import "gui/canvas/lisp.inc")
(import "gui/vdu/lisp.inc")
