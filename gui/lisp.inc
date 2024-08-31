;import symbols
(import "lib/consts/colors.inc")

;lisp bindings
(import "./view/lisp.inc")
(import "./window/lisp.inc")
(import "./backdrop/lisp.inc")
(import "./slider/lisp.inc")
(import "./progress/lisp.inc")
(import "./scroll/lisp.inc")
(import "./textfield/lisp.inc")
(import "./text/lisp.inc")
(import "./title/lisp.inc")
(import "./grid/lisp.inc")
(import "./flow/lisp.inc")
(import "./stroke/lisp.inc")
(import "./stack/lisp.inc")
(import "./tree/lisp.inc")
(import "./vdu/lisp.inc")
(import "./button/lisp.inc")
(import "./node/lisp.inc")
(import "./label/lisp.inc")
(import "./radiobar/lisp.inc")
(import "./path/lisp.inc")
(import "./canvas/lisp.inc")
(import "./texture/lisp.inc")
(import "./vdu/lisp.inc")
(import "./font/lisp.inc")
(import "./pixmap/lisp.inc")
(import "./edit/lisp.inc")
(import "./spinner/lisp.inc")
(import "./hchart/lisp.inc")

;ui tree helper macros
(defun ui-merge-props (p)
	; (ui-merge-props props) -> props
	(defq ps (clear '()) pv (clear '()))
	(each! (lambda (s v)
		(cond
			((defq i (find-rev s ps))
				(elem-set ps i s) (elem-set pv i v))
			(:t (push ps s) (push pv v))))
		(unzip (opt p '()) 2))
	(zip ps pv))

(defmacro ui-props (p &rest x)
	; (ui-props props [props]) -> props
	`(setq ,p (cat ',x (opt ,p '()))))

(defmacro ui-root (n c &optional p &rest x)
	; (ui-root name constructor [props] [body]) -> view
	(setq p (ui-merge-props p))
	(if (/= 0 (length p))
		`(progn (defq _ui (list (defq ,n ,c))) (def ,n ~p) ~x (setq _ui :nil))
		`(progn (defq _ui (list (defq ,n ,c))) ~x (setq _ui :nil))))

(defmacro ui-element (n c &optional p &rest x)
	; (ui-element name constructor [props] [body]) -> view
	(setq p (ui-merge-props p))
	(if (/= 0 (length p))
		`(progn (. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n) (def ,n ~p) ~x (pop _ui))
		`(progn (. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n) ~x (pop _ui))))

(defmacro ui-flow (n &optional p &rest x)
	; (ui-flow name [props] [body]) -> flow
	`(ui-element ,n (Flow) ,p ~x))

(defmacro ui-grid (n &optional p &rest x)
	; (ui-grid name [props] [body]) -> grid
	`(ui-element ,n (Grid) ,p ~x))

(defmacro ui-stack (n t &optional p &rest x)
	; (ui-stack name tabs [props] [body]) -> stack
	`(ui-element ,n (Stack ,t) ,p ~x))

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

(defmacro ui-tree (n e &optional p)
	; (ui-tree name event [props]) -> tree
	`(ui-element ,n (Tree ,e) ,p))

(defmacro ui-spinner (n &optional p)
	; (ui-spinner name [props]) -> spinner
	`(ui-element ,n (Spinner) ,p))

(defmacro ui-hchart (n t m &optional p)
	; (ui-hchart name title num_marks [props]) -> hchart
	`(ui-element ,n (Hchart ,t ,m) ,p))

(defmacro ui-title (n &optional p)
	; (ui-title name [props]) -> title
	(ui-props p
		:font *env_title_font*
		:border *env_title_border*)
	`(ui-element ,n (Title) ,p))

(defmacro ui-button (n &optional p &rest x)
	; (ui-button name [props] [body]) -> button
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_down +flow_flag_align_hcenter +flow_flag_align_vcenter))
		:border *env_button_border*)
	`(ui-element ,n (Button) ,p ~x))

(defmacro ui-buttons (s e &optional p)
	; (ui-buttons symbols event [props])
	(setq s (map (# (if (num? %0) (num-to-utf8 %0) %0)) s))
	(ui-props p
		:text %0)
	`(each (# (. (ui-button __ ,p) :connect (+ (!) ,e))) '(~s)))

(defmacro ui-title-bar (n s b e &optional p)
	; (ui-title-bar name title symbols event [props]) -> flow
	(ui-props p
		:flow_flags +flow_left_fill
		:color *env_title_col*
		:border *env_title_buttons_border*
		:font *env_title_buttons_font*)
	`(ui-flow _ ,p
		(ui-buttons ,b ,e)
		(ui-title ,n (:text ,s))))

(defmacro ui-text (n &optional p)
	; (ui-text name [props]) -> text
	`(ui-element ,n (Text) ,p))

(defmacro ui-stroke (n &optional p)
	; (ui-text name [props]) -> stroke
	`(ui-element ,n (Stroke) ,p))

(defmacro ui-label (n &optional p &rest x)
	; (ui-label name [props] [body]) -> label
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_right +flow_flag_align_vcenter))
		:border *env_label_border*)
	`(ui-element ,n (Label) ,p ~x))

(defmacro ui-radio-bar (n s &optional p)
	; (ui-radio-bar name symbols [props])
	(ui-props p
		:color *env_toolbar_col*
		:font *env_toolbar_font*
		:border *env_button_border*)
	`(ui-element ,n (Radiobar '(~s)) ,p))

(defmacro ui-toggle-bar (n s &optional p)
	; (ui-toggle-bar name symbols [props])
	(ui-props p
		:color *env_toolbar_col*
		:font *env_toolbar_font*
		:border *env_button_border*)
	`(ui-element ,n (Radiobar '(~s) :t) ,p))

(defmacro ui-tool-bar (n &optional p &rest x)
	; (ui-tool-bar name [props] [body]) -> flow
	(ui-props p
		:flow_flags +flow_right
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

(defun ui-tool-tips (view tips)
	; (ui-tool-tips view tips)
	(if (Stack? view) (setq view (get :tab_flow view)))
	(each (# (def %0 :tip_text %1)) (. view :children) tips))

;; (defun ui-save (stream view)
;;     (defq tree (scatter (Lmap) :type (last (. view :type_of))) stack (list view tree))
;;     (while (defq props (pop stack) view (pop stack))
;;         (each (lambda ((k v))
;;             (unless (View? v)
;;                 (if (eql k :font) (setq v (font-info v)))
;;                 (. props :insert k v))) (tolist view))
;;         (when (nempty? (defq children (. view :children)))
;;             (. props :insert :children (defq child_props (list)))
;;             (each (# (push stack %0 (defq props (scatter (Lmap) :type (last (. %0 :type_of)))))
;;                     (push child_props props)) children)))
;;     (tree-save stream tree))
