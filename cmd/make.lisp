;imports
(import 'cmd/asm.inc)
(import 'cmd/options.inc)

(defun-bind make-doc ()
	(defq *abi* (abi) *cpu* (cpu))
	(defun-bind trim-whitespace (_)
		(trim-start (trim-start _ (const (ascii-char 9))) " "))
	(defun-bind trim-cruft (_)
		(sym (trim-start (trim-end _ ")") "'")))
	(defun-bind trim-parens (_)
		(sym (trim-start (trim-end _ ")") "(")))
	(defun-bind chop (_)
		(when (defq e (find (char 0x22) _))
			(setq _ (slice 0 e _))
			(sym (slice (inc (find (char 0x22) _)) -1 _))))
	(print "Scanning source files...")
	(defq *imports* (list 'make.inc) classes (list) functions (list)
		docs (list) syntax (list) state 'x)
	(within-compile-env (lambda ()
		(include 'sys/func.inc)
		(each include (all-class-files))
		(each-mergeable (lambda (_)
			(each-line (lambda (_)
				(setq _ (trim-end _ (const (ascii-char 13))))
				(defq l (trim-whitespace _))
				(when (eql state 'y)
					(if (and (>= (length l) 1) (eql (elem 0 l) ";"))
						(push (elem -2 docs) (slice 1 -1 l))
						(setq state 'x)))
				(when (and (eql state 'x) (>= (length l) 10) (eql "(" (elem 0 l)))
					(defq s (split l " ") _ (elem 0 s))
					(cond
						((eql _ "(include")
							(merge *imports* (list (trim-cruft (elem 1 s)))))
						((eql _ "(def-class")
							(push classes (list (trim-cruft (elem 1 s))
								(if (= 3 (length s)) (trim-cruft (elem 2 s)) 'null))))
						((eql _ "(dec-method")
							(push (elem -2 classes) (list (trim-cruft (elem 1 s)) (trim-cruft (elem 2 s)))))
						((eql _ "(def-method")
							(setq state 'y)
							(push docs (list))
							(push functions (f-path (trim-cruft (elem 1 s)) (trim-cruft (elem 2 s)))))
						((or (eql _ "(def-func") (eql _ "(defcfun") (eql _ "(defcfun-bind"))
							(setq state 'y)
							(push docs (list))
							(push functions (trim-cruft (elem 1 s))))
						((and (or (eql _ "(call") (eql _ "(jump")) (eql (elem 2 s) "'repl_error"))
							(if (setq l (chop l))
								(merge syntax (list l))))))) (file-stream _))) *imports*)))
	;create classes docs
	(sort (lambda (x y)
		(cmp (elem 0 x) (elem 0 y))) classes)
	(defq stream (string-stream (cat "")))
	(write-line stream (const (str "# Classes" (ascii-char 10))))
	(each (lambda ((class super &rest methods))
		(write-line stream (cat "## " class (const (ascii-char 10))))
		(write-line stream (cat "Super Class: " super (const (ascii-char 10))))
		(each (lambda ((method function))
			(write-line stream (cat "### " class "::" method " -> " function (const (ascii-char 10))))
			(when (and (defq i (find function functions))
					(/= 0 (length (elem i docs))))
				(write-line stream "```lisp")
				(each (lambda (_)
					(write-line stream _)) (elem i docs))
				(write-line stream (const (str "```" (ascii-char 10)))))) methods)) classes)
	(save (str stream) 'docs/CLASSES.md)
	(print "-> docs/CLASSES.md")

	;create lisp syntax docs
	(each (lambda (_)
		(each-line (lambda (_)
			(setq _ (trim-end _ (const (ascii-char 13))))
			(defq l (trim-whitespace _))
			(when (eql state 'y)
				(if (and (>= (length l) 1) (eql (elem 0 l) ";"))
					(merge syntax (list (sym (slice 1 -1 l))))
					(setq state 'x)))
			(when (and (eql state 'x) (>= (length l) 10) (eql "(" (elem 0 l)))
				(defq s (split l " ") _ (elem 0 s))
				(cond
					((or (eql _ "(defun") (eql _ "(defmacro") (eql _ "(defun-bind") (eql _ "(defmacro-bind"))
						(setq state 'y))))) (file-stream _)))
		'(class/in/lisp.inc class/lisp.inc class/lisp/anaphoric.inc class/lisp/boot.inc
			class/lisp/debug.inc class/out/lisp.inc class/slave/lisp.inc
			gui/canvas/lisp.inc gui/lisp.inc gui/points/lisp.inc gui/view/lisp.inc
			gui/window/lisp.inc sys/lisp.inc class/num/lisp.inc))
	(sort cmp syntax)
	(defq stream (string-stream (cat "")))
	(write-line stream (const (str "# Syntax" (ascii-char 10))))
	(each (lambda (_)
		(defq s (split _ " ") form (trim-parens (elem 0 s)))
		(when (eql "(" (elem 0 (elem 0 s)))
			(write-line stream (cat "## " form (const (ascii-char 10))))
			(write-line stream (cat _ (const (ascii-char 10)))))) syntax)
	(save (str stream) 'docs/SYNTAX.md)
	(print "-> docs/SYNTAX.md"))

(defun-bind make-syms ()
	(print "Scanning source files...")
	(defq stream (string-stream (cat "")))
	(defq _syms_
		'(argb_black argb_blue argb_cyan argb_green argb_grey1 argb_grey10 argb_grey11
		argb_grey12 argb_grey13 argb_grey14 argb_grey15 argb_grey2 argb_grey3
		argb_grey4 argb_grey5 argb_grey6 argb_grey7 argb_grey8 argb_grey9 argb_magenta
		argb_red argb_white argb_yellow byte_size canvas_color canvas_flags
		cap_arrow cap_butt cap_round cap_square cap_tri component_id
		ev_msg_action_source_id ev_msg_key_key ev_msg_key_keycode ev_msg_mouse_buttons
		ev_msg_mouse_rx ev_msg_mouse_ry ev_msg_target_id ev_msg_type ev_type_gui
		ev_type_key ev_type_mouse flow_flag_align_hcenter flow_flag_align_hleft
		flow_flag_align_hright flow_flag_align_vbottom flow_flag_align_vcenter
		flow_flag_align_vtop flow_flag_down flow_flag_fillh flow_flag_fillw
		flow_flag_lasth flow_flag_lastw flow_flag_left flow_flag_right flow_flag_up
		in_mbox_id in_state int_size join_bevel join_miter join_round kn_call_child
		kn_call_open load_flag_film load_flag_noswap load_flag_shared long_size
		out_state ptr_size scroll_flag_horizontal scroll_flag_vertical short_size
		slave_args slider_col status_col stream_mail_state_started
		stream_mail_state_stopped stream_mail_state_stopping title_col toolbar_col
		toolbar2_col vdu_char_height vdu_char_width view_flag_at_back
		view_flag_dirty_all view_flag_opaque view_flag_solid
		view_flags view_h view_w view_x view_y window_close window_col
		window_flag_close window_flag_max window_flag_min window_flag_status window_max
		window_min window_status window_title)
		_vals_
		(within-compile-env (lambda ()
			(include 'sys/kernel/class.inc)
			(include 'sys/list/class.inc)
			(include 'sys/mail/class.inc)
			(include 'class/in/class.inc)
			(include 'class/out/class.inc)
			(include 'class/slave/class.inc)
			(include 'gui/gui/class.inc)
			(include 'gui/ctx/class.inc)
			(include 'gui/flow/class.inc)
			(include 'gui/vdu/class.inc)
			(include 'gui/window/class.inc)
			(include 'gui/scroll/class.inc)
			(include 'gui/canvas/class.inc)
			(include 'gui/points/class.inc)
			(map eval _syms_))))
	(write-line stream ";;;;;;;;;;;;")
	(write-line stream "; VP symbols")
	(write-line stream ";;;;;;;;;;;;")
	(write-line stream "(defq")
	(each (lambda (s v)
		(write stream s)
		(write-char stream (const (ascii-code " ")))
		(write-line stream (str v))) _syms_ _vals_)
	(write-line stream ")")
	(save (str stream) 'sys/symbols.inc)
	(print "-> sys/symbols.inc"))

(defq usage `(
(("-h" "--help")
"Usage: make [options] [all] [boot] [platforms] [doc] [syms]
	options:
		-h --help: this help info.
	all: include all .vp files.
	boot: create a boot image.
	platforms: for all platforms not just the host.
	doc: scan source files and create documentation.")
))

;initialize pipe details and command args, abort on error
(when (and (defq slave (create-slave)) (defq args (options slave usage)))
	(defq args (map sym args) all (find 'all args) boot (find 'boot args) platforms (find 'platforms args)
		doc (find 'doc args) syms (find 'syms args))
	(cond
		((and boot all platforms) (remake-all-platforms))
		((and boot all) (remake-all))
		((and boot platforms) (remake-platforms))
		((and all platforms) (make-all-platforms))
		(all (make-all))
		(platforms (make-platforms))
		(boot (remake))
		(doc (make-doc))
		(syms (make-syms))
		(t (make))))
