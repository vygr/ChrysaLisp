;import symbols
(import 'sys/symbols.inc)

;some helpful macros
(defmacro get (_ b) `(eval ,b ,_))

(defmacro ui-tree (n c &optional p &rest x)
	(if p
		`(progn
			(defq _ui (list (defq ,n ,c)))
			(def ,n ~p)
			~x (setq _ui nil))
		`(progn
			(defq _ui (list (defq ,n ,c)))
			~x (setq _ui nil))))

(defmacro ui-element (n c &optional p &rest x)
	(if p
		`(progn
			(view-add-child (elem -3 (push _ui (defq ,n ,c))) ,n)
			(def ,n ~p)
			~x (pop _ui))
		`(progn
			(view-add-child (elem -3 (push _ui (defq ,n ,c))) ,n)
			~x (pop _ui))))

(defmacro ui-buttons (s e &optional c f)
	(setq s (map (lambda (_) (if (num? _) (num-to-utf8 _) _)) s))
	(cond
		((and c f)
			`(each (lambda (_s)
				(component-connect (ui-element __ (create-button) ('text _s 'color ,c 'font ,f)) (+ _ ,e))) '(~s)))
		(c	`(each (lambda (_s)
				(component-connect (ui-element __ (create-button) ('text _s 'color ,c)) (+ _ ,e))) '(~s)))
		(f	`(each (lambda (_s)
				(component-connect (ui-element __ (create-button) ('text _s 'font ,f)) (+ _ ,e))) '(~s)))
		(t	`(each (lambda (_s)
				(component-connect (ui-element __ (create-button) ('text _s)) (+ _ ,e))) '(~s)))))

(defmacro view-dirty-all (_)
	`(view-set-flags ,_ view_flag_dirty_all view_flag_dirty_all))

;useful flow combos
(defq flow_down_fill (logior flow_flag_down flow_flag_fillw flow_flag_lasth)
	flow_right_fill (logior flow_flag_right flow_flag_fillh flow_flag_lastw)
	flow_left_fill (logior flow_flag_left flow_flag_fillh flow_flag_lastw))

;lisp bindings
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
(ffi create-font "gui/font/lisp_create" 0)
(ffi font-glyph-paths "gui/font/lisp_glyph_paths" 0)
(ffi font-glyph-ranges "gui/font/lisp_glyph_ranges" 0)
(import 'gui/component/lisp.inc)
(import 'gui/view/lisp.inc)
(import 'gui/points/lisp.inc)
(import 'gui/canvas/lisp.inc)
(import 'gui/vdu/lisp.inc)
