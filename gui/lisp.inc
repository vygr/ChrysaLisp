;import symbols
(import "lib/consts/colors.inc")
(import "lib/consts/symbols.inc")
(import "lib/theme/theme.inc")

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
(import "./files/lisp.inc")
(import "./hchart/lisp.inc")
(import "./mask/lisp.inc")
(import "./md/lisp.inc")

;What a view does not set itself it finds above it, as a name is found in
;an environment. Above every window is the screen, with the properties of
;the user, usr/env.inc *env_props*, item 118. A view draws and is sized by
;the plain ones, :font :paper :ink :border, a variant is a plain name with
;more after it, :paper_window, and a view given a variant for a plain one,
;(:paper :paper_window), has that variant's value as it is found from it.
;Chris: "an individual widget work on plain properties like :font :color
;:ink :paper etc, that's what they actually use to redraw and size, but the
;variants can be there as well"

(defq +variant_keys ''(:font :paper :ink :border :shadow))

(defun env-props ()
	; (env-props) -> ((key val) ...)
	;the properties of views in the user's *env*, usr/env.inc, the plain
	;names and their variants, those the screen has. The screen draws
	;itself by its own :paper and :ink, so a window chooses its own from
	;:paper_window and :ink_window. Each task's own, *env_props*
	(unless *env_props*
		(setq *env_props* (filter (lambda ((key &ignore))
				(defq name (str key))
				(some (# (starts-with (str %0) name)) +variant_keys))
			(tolist *env*))))
	*env_props*)

(defun env-root ()
	; (env-root) -> view
	;the user's properties on a view of this task's own, the root of each
	;window it makes till it is shown, (window-attach). The task's own as
	;usr/env.inc makes *env_root* in each task: gui/lisp.inc is in the
	;shared root of a GUI's node, and a name is found where it is used
	(unless *env_root*
		(setq *env_root* (View))
		(each (lambda ((key val)) (def *env_root* key val)) (env-props)))
	*env_root*)

(defq +theme_fonts ''(:font_title_buttons :font_symbol :font_medium_symbol :font_small_toolbar :font_warning))

(defun env-theme (name)
	; (env-theme name) -> ((old new) ...)
	;the symbol fonts of the user's *env* are those of the theme, each the
	;size it was, and so on this task's root. Those that changed, the font
	;that was and the one that is
	(defq file (theme-file name) swaps (list))
	(def *env* :symbol_file file)
	(each (lambda (key)
			(when (defq old (get key *env*))
				(defq new (create-font file (second (font-info old))))
				(unless (eql new old)
					(def *env* key new)
					(push swaps (list old new)))))
		+theme_fonts)
	(setq *env_props* :nil)
	(if *env_root* (each (lambda ((key val)) (def *env_root* key val)) (env-props)))
	swaps)

(defun view-resolve (view)
	; (view-resolve view) -> view
	;each view of it, it too, given a variant for a plain property has the
	;variant's value, found from where it is. Which variant it was is
	;kept, :variants, so a new theme or user can be found again by it
	(each (lambda (v)
			(defq kept (ifn (def? :variants v) (list)))
			(each (lambda (key)
					(and (defq val (def? key v)) (sym? val) (starts-with (cat (str key) "_") (str val))
						(push kept key val)))
				+variant_keys)
			(when (nempty? kept)
				(def v :variants kept)
				(each (lambda ((key variant))
						(if (defq found (get variant v)) (def v key found) (undef v key)))
					(partition kept 2))))
		(. view :flatten))
	view)

(defun window-attach (window)
	; (window-attach window) -> window
	;a window is put on the user's root of its own task as it is made, so
	;what it does not set it finds there as it is laid out, and its
	;variants are found. Shown, it goes onto the screen, which has the same
	;properties. Not the screen before then: a window an app made and did
	;not show would be left on it when the app has gone, and its class
	;with the app, the login app's of a session of a user say
	(. (env-root) :add_back window)
	(view-resolve window))

;ui tree helper macros
(defun ui-merge-props (p)
	; (ui-merge-props props) -> props
	(defq ps (clear '()) pv (clear '()) pe (clear '()))
	(each (lambda ((s v))
		(cond
			((eql v :erase)
				(push pe s)
				(when (defq i (find s ps))
					(elem-set ps i (last ps)) (pop ps)
					(elem-set pv i (last pv)) (pop pv)))
			((defq i (find s ps))
				(elem-set ps i s) (elem-set pv i v))
			((push ps s) (push pv v))))
		(if p (partition p 2) '()))
	;a variant given for a plain property, (:paper :paper_toolbar), is the
	;user's, (get :paper_toolbar *env*), as the view is made, and which it
	;was is kept, :variants, for a theme to find it again, (view-resolve)
	(defq kept (list))
	(each (lambda (s v)
			(when (and (find s +variant_keys) (sym? v) (starts-with (cat (str s) "_") (str v)))
				(elem-set pv (!) (list 'get v '*env*))
				(push kept s v)))
		ps pv)
	(when (nempty? kept)
		(push ps :variants) (push pv (list 'quote kept)))
	(list (zip ps pv) pe))

(defmacro ui-props (p &rest x)
	; (ui-props props [props]) -> props
	(static-qq (setq ,p (cat ',x (ifn ,p '())))))

(defmacro ui-root (n c &optional p &rest x)
	; (ui-root name constructor [props] [body]) -> view
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

(defmacro ui-element (n c &optional p &rest x)
	; (ui-element name constructor [props] [body]) -> view
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

(defmacro ui-flow (n &optional p &rest x)
	; (ui-flow name [props] [body]) -> flow
	(static-qqp (ui-element ,n (Flow) ,p ~x)))

(defmacro ui-grid (n &optional p &rest x)
	; (ui-grid name [props] [body]) -> grid
	(static-qqp (ui-element ,n (Grid) ,p ~x)))

(defmacro ui-stack (n t &optional p &rest x)
	; (ui-stack name tabs [props] [body]) -> stack
	(static-qqp (ui-element ,n (Stack ,t) ,p ~x)))

(defmacro ui-window (n &optional p &rest x)
	; (ui-window name [props] [body]) -> window
	;a window chooses its plain properties from the variants on the
	;screen, which draws itself by its own
	(ui-props p
		:font :font_window
		:ink :ink_window
		:paper :paper_window
		:border :border_window
		:shadow :shadow_window
		:zoom 1.0)
	(static-qqp (progn (ui-root ,n (Window) ,p
		(ui-flow _ (:flow_flags +flow_down_fill) ~x)) (window-attach ,n))))

(defmacro ui-files (n t e &optional p)
	; (ui-files name title event [props]) -> files
	(static-qqp (ui-element ,n (Files ,t ,e) ,p)))

(defmacro ui-spinner (n &optional p)
	; (ui-spinner name [props]) -> spinner
	(static-qqp (ui-element ,n (Spinner) ,p)))

(defmacro ui-hchart (n t m &optional p)
	; (ui-hchart name title num_marks [props]) -> hchart
	(static-qqp (ui-element ,n (Hchart ,t ,m) ,p)))

(defmacro ui-title (n &optional p)
	; (ui-title name [props]) -> title
	(ui-props p
		:font :font_title
		:border :border_title)
	(static-qqp (ui-element ,n (Title) ,p)))

(defmacro ui-button (n &optional p &rest x)
	; (ui-button name [props] [body]) -> button
	(ui-props p
		:flow_flags (bit-mask +flow_flag_down +flow_flag_align_hcenter +flow_flag_align_vcenter)
		:border :border_button)
	(static-qqp (ui-element ,n (Button) ,p ~x)))

(defun ui-symbols (s)
	; (ui-symbols symbols) -> strs
	;what a bar's buttons show, as text. A symbol of the symbol font by its
	;name, +sym_undo, lib/consts/symbols.inc, or by its code, and a str is
	;itself. A name is in a list that is not run, so it is looked up here
	(map (# (cond
		((num? %0) (num-to-utf8 %0))
		((and (sym? %0) (starts-with "+sym_" %0)) (num-to-utf8 (eval %0)))
		(%0))) s))

(defmacro ui-buttons (s e &optional p)
	; (ui-buttons symbols event [props])
	(setq s (ui-symbols s))
	(ui-props p
		:text %0)
	(static-qqp (each! (# (. (ui-button __ ,p) :connect (if (< ,e 0) ,e (+ (!) ,e)))) (list '(~s)))))

(defmacro ui-title-bar (n s b e &optional p)
	; (ui-title-bar name title symbols event [props]) -> flow
	(ui-props p
		:flow_flags +flow_left_fill
		:paper :paper_title
		:border :border_title_buttons
		:font :font_title_buttons)
	(static-qqp (ui-flow _ ,p
		(ui-buttons ,b ,e)
		(ui-title ,n (:text ,s)))))

(defmacro ui-text (n &optional p)
	; (ui-text name [props]) -> text
	(static-qqp (ui-element ,n (Text) ,p)))

(defmacro ui-stroke (n &optional p)
	; (ui-stroke name [props]) -> stroke
	(static-qqp (ui-element ,n (Stroke) ,p)))

(defmacro ui-label (n &optional p &rest x)
	; (ui-label name [props] [body]) -> label
	(ui-props p
		:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter)
		:border :border_label)
	(static-qqp (ui-element ,n (Label) ,p ~x)))

(defmacro ui-radio-bar (n s &optional p)
	; (ui-radio-bar name symbols [props])
	(ui-props p
		:paper :paper_toolbar
		:font :font_symbol
		:border :border_button)
	(setq s (ui-symbols s))
	(static-qqp (ui-element ,n (Radiobar '(~s)) ,p)))

(defmacro ui-toggle-bar (n s &optional p)
	; (ui-toggle-bar name symbols [props])
	(ui-props p
		:paper :paper_toolbar
		:font :font_symbol
		:border :border_button)
	(setq s (ui-symbols s))
	(static-qqp (ui-element ,n (Radiobar '(~s) :t) ,p)))

(defmacro ui-tool-bar (n &optional p &rest x)
	; (ui-tool-bar name [props] [body]) -> flow
	(ui-props p
		:flow_flags +flow_right
		:paper :paper_toolbar :font :font_symbol)
	(static-qqp (ui-flow ,n ,p ~x)))

(defmacro ui-textfield (n &optional p)
	; (ui-textfield name [props]) -> textfield
	(ui-props p
		:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter)
		:border :border_textfield)
	(static-qqp (ui-element ,n (Textfield) ,p)))

(defmacro ui-slider (n &optional p)
	; (ui-slider name [props]) -> slider
	(ui-props p
		:paper :paper_slider)
	(static-qqp (ui-element ,n (Slider) ,p)))

(defmacro ui-scroll (n f &optional p &rest x)
	; (ui-scroll name flags [props] [body]) -> scroll
	(ui-props p
		:paper :paper_slider)
	(static-qqp (ui-element ,n (Scroll ,f) ,p ~x)))

(defmacro ui-backdrop (n &optional p &rest x)
	; (ui-backdrop name [props] [body]) -> backdrop
	(static-qqp (ui-element ,n (Backdrop) ,p ~x)))

(defmacro ui-md (n &optional lines p &rest x)
	; (ui-md name [text_lines] [props] [body]) -> md
	(ui-props p
		:flow_flags +flow_down_fill)
	(static-qqp (ui-element ,n (Md ,lines) ,p ~x)))

(defmacro ui-progress (n &optional p)
	; (ui-progress name [props]) -> progress
	(static-qqp (ui-element ,n (Progress) ,p)))

(defmacro ui-canvas (n w h s &optional p)
	; (ui-canvas name width height scale [props]) -> canvas
	(ui-props p
		:paper 0)
	(static-qqp (ui-element ,n (Canvas ,w ,h ,s) ,p)))

(defmacro ui-vdu (n &optional p)
	; (ui-vdu name [props]) -> vdu
	(ui-props p
		:font :font_terminal
		:paper 0)
	(static-qqp (ui-element ,n (Vdu) ,p)))

(defmacro ui-view (n &optional p &rest x)
	; (ui-view name [props] [body]) -> view
	(static-qqp (ui-element ,n (View) ,p ~x)))

(defun ui-tool-tips (view tips)
	; (ui-tool-tips view tips)
	(if (Stack? view) (setq view (. view :get_radio_bar)))
	(each (# (def %0 :tip_text %1)) (. view :children) tips))

(defun ui-save-atom (v)
	; (ui-save-atom atom) -> atom
	(if (and (sym? v) (or (not (starts-with ":" v)) (< (length v) 2)
			(some (# (bfind %0 (const (char-class " \t\r\n\q'(),;[]`{}")))) v)))
		(str v) v))

(defun ui-save (stream view &optional depth keys)
	; (ui-save stream view [depth keys]) -> tree | :nil
	;the tree of a view and those in it, each its :type, its :bounds in
	;its parent, and its properties: atoms, flat sequences of atoms, fonts
	;as (name pixels). depth the levels of children saved, :nil for all, a
	;view at the last has :children_count in place of its children, as has
	;a subtree in it, the child of a Scroll, a page of the Docs app say,
	;unless it is the view saved. keys the properties saved, :nil for all.
	;A symbol not a plain :keyword is saved as its text, as the words of an Md
	;are symbols, with spaces and brackets in them, that a tree writes bare
	;and could not read back
	(defq tree (Lmap) stack (list view tree 0))
	(while (defq level (pop stack) props (pop stack) view (pop stack))
		(. props :insert :type (last (. view :type_of)))
		(. props :insert :bounds (. view :get_bounds))
		(each (lambda ((k v))
			(when (or (not keys) (find k keys))
				(cond
					((eql k :font) (. props :insert k (font-info v)))
					((or (sym? v) (num? v) (str? v)) (. props :insert k (ui-save-atom v)))
					((and (seq? v) (every (# (or (sym? %0) (num? %0) (str? %0))) v))
						(. props :insert k (if (some (const sym?) v) (map (const ui-save-atom) v) v))))))
			(tolist view))
		(cond
			((empty? (defq children (. view :children))))
			((or (and depth (>= level depth))
					(and (> level 0) (bits? (. view :get_flags) +view_flag_subtree)))
				(. props :insert :children_count (length children)))
			(:t (. props :insert :children (reduce (lambda (p child)
					(push stack child (defq child_props (Lmap)) (inc level))
					(push p child_props))
				children (list))))))
	(tree-save stream tree))

(defun ui-find (view key value)
	; (ui-find view key value) -> :nil | view
	;the first of view and those in it, depth first, whose key is value, a
	;:type by its name, Button or :Button, any other as it prints
	(defq todo (list view) out :nil value (str value))
	(while (and (not out) (defq v (pop todo)))
		(if (if (eql key :type)
				(find (sym (cat ":" (if (starts-with ":" value) (slice value 1 -1) value))) (. v :type_of))
				(and (def? key v) (eql (str (get key v)) value)))
			(setq out v)
			(reach (# (push todo %0)) (. v :children))))
	out)
