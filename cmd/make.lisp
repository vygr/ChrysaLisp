;imports
(import "lib/asm/asm.inc")
(import "lib/pipe/pipe.inc")
(import "lib/options/options.inc")

(defun boot-funcs ()
	(defq out (list) e (penv))
	(while (penv e) (setq e (penv e)))
	(each (lambda ((k v))
		(if (func? v) (push out k))) (tolist e))
	(sort cmp out))

(defun boot-lambdas ()
	(defq out (list) e (penv))
	(while (penv e) (setq e (penv e)))
	(each (lambda ((k v))
		(if (lambda? v) (push out k))) (tolist e))
	(sort cmp out))

(defun boot-macros ()
	(defq out (list) e (penv))
	(while (penv e) (setq e (penv e)))
	(each (lambda ((k v))
		(if (macro? v) (push out k))) (tolist e))
	(sort cmp out))

(defun make-syms ()
	(defq *abi* (abi) *cpu* (cpu))
	(print "Scanning source files...")
	(defq _t_syms_ (list) _t_syms_vals_ (list) _syms_ '(
			+cap_butt+ +cap_round+ +cap_square+ +cap_tri+ view_id ev_msg_action_source_id
			ev_msg_key_key ev_msg_key_keycode ev_msg_mouse_buttons ev_msg_mouse_rx
			ev_msg_mouse_ry ev_msg_target_id ev_msg_type ev_type_gui ev_type_key
			ev_type_mouse file_open_append file_open_read file_open_write in_mbox_id
			in_state +join_bevel+ +join_miter+ +join_round+ kn_call_child kn_call_open
			stdio_args stream_mail_state_started stream_mail_state_stopped
			stream_mail_state_stopping vdu_char_height vdu_char_width view_flags view_h
			view_w view_x view_y canvas_flags canvas_color +cap_arrow+ canvas_scale
			canvas_pixmap pixmap_width pixmap_height ev_msg_mouse_x ev_msg_mouse_y
			ev_type_action view_target_ids canvas_texture texture_handle texture_width
			texture_height net_id_size node_id_size)
		_vals_ (within-compile-env (lambda ()
	        (include "sys/func.inc")
			(each include (all-class-files))
			(map (#
				(defq st (sym (cat "_t_" %0)))
				(when (defq stv (get st))
					(push _t_syms_ st)
					(push _t_syms_vals_ stv))
				(eval %0)) _syms_)))
		stream (file-stream "sys/symbols.inc" file_open_write))
	(write-line stream ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
	(write-line stream "; VP symbols, autogen do not edit !")
	(write-line stream ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
	(write-line stream "(defq")
	(each (lambda (s v)
		(write-line stream (cat s " " (str v)))) _syms_ _vals_)
	(each (lambda (s v)
		(write-line stream (cat s " " (ascii-char 34) v (ascii-char 34)))) _t_syms_ _t_syms_vals_)
	(write-line stream ")")
	(print "-> sys/symbols.inc") nil)

(defun make-docs ()
    (defq *abi* (abi) *cpu* (cpu))
    (defun chop (_)
        (when (defq e (find-rev (char 0x22) _))
            (setq _ (slice 0 e _))
            (slice (inc (find-rev (char 0x22) _)) -1 _)))
    (print "Scanning source files...")
    (defq *imports* (all-vp-files) classes (list) functions (list) docs (list) syntax (list) state :x)
    (within-compile-env (lambda ()
        (include "sys/func.inc")
        (each include (all-class-files))
        (each-mergeable (lambda (file)
            (each-line (lambda (line)
                (when (eql state :y)
                    (defq s (split line (const (cat (ascii-char 9) " "))))
                    (if (and (> (length s) 0) (starts-with ";" (elem 0 s)))
                        (push (elem -2 docs) (slice (inc (find ";" line)) -1 line))
                        (setq state :x)))
                (when (and (eql state :x) (>= (length line) 9))
                    (defq s (split line (const (cat (ascii-char 9) " ()'" (ascii-char 34) (ascii-char 13)))) _ (elem 0 s))
                    (cond
                        ((eql _ "include")
                            (make-merge *imports* (list (elem 1 s))))
                        ((eql _ "def-class")
                            (push classes (list (elem 1 s) (if (= 3 (length s)) (elem 2 s) "null"))))
                        ((eql _ "dec-method")
                            (push (elem -2 classes) (list (elem 1 s) (sym (elem 2 s)))))
                        ((eql _ "def-method")
                            (setq state :y)
                            (push docs (list))
                            (push functions (f-path (sym (elem 1 s)) (sym (elem 2 s)))))
                        ((or (eql _ "def-func") (eql _ "defun") (eql _ "defun-unbound"))
                            (setq state :y)
                            (push docs (list))
                            (push functions (sym (elem 1 s))))
                        ((and (or (eql _ "call") (eql _ "jump")) (eql (elem 2 s) ":repl_error"))
                            (if (setq line (chop line))
                                (merge-obj syntax (list (sym line)))))))) (file-stream file))) *imports*)))
    ;create classes docs
    (sort (# (cmp (elem 0 %0) (elem 0 %1))) classes)
    (defq stream (file-stream "docs/VP_CLASSES.md" file_open_write))
    (write-line stream (const (str "# VP Classes" (ascii-char 10))))
    (each (lambda ((cls super &rest methds))
        (write-line stream (cat "## " cls (ascii-char 10)))
        (write-line stream (cat "Super Class: " super (ascii-char 10)))
        (each (lambda ((methd function))
            (write-line stream (cat "### " cls " " methd " -> " function (ascii-char 10)))
            (when (and (defq i (find function functions)) (/= 0 (length (elem i docs))))
                (write-line stream "```lisp")
                (each (# (write-line stream %0)) (elem i docs))
                (write-line stream (const (str "```" (ascii-char 10)))))) methds)) classes)
    (print "-> docs/VP_CLASSES.md")

	;create commands docs
	(defq target 'docs/COMMANDS.md)
	(defun extract-cmd (el)
		(first (split (second (split el "/")) ".")))
	(defun cmd-collector (acc el)
		(push acc (list (sym el) (str el " -h"))))
	(defun wrap-block (content)
		(if (/= (length content) 0)
		(str (const (pad (ascii-char 10) 2))
		"```lisp" (ascii-char 10)
		content (ascii-char 10)
		"```" (ascii-char 10))
		""))
	(defun generate-cmd-help (lst)
		(defq _eat_chunk "")
		(defun _eat (_x)
		(setq _eat_chunk (cat _eat_chunk (wrap-block _x))))
		(each (lambda (el)
		(setq _eat_chunk (cat _eat_chunk (str "## " (first el) (const (ascii-char 10)))))
		(pipe-run (second el) _eat)) lst)
		(save _eat_chunk target))
	(generate-cmd-help (reduce
		cmd-collector
		(sort cmp (map extract-cmd (all-files "cmd" ".lisp")))
		(list)))
	(print "-> docs/COMMANDS.md")

    ;create lisp syntax docs
    (each (lambda (file)
        (each-line (lambda (line)
            (when (eql state :y)
				(defq s (split line (const (cat (ascii-char 9) " "))))
				(if (and (> (length s) 1) (starts-with ";" (elem 0 s)) (starts-with "(" (elem 1 s)))
					(merge-obj syntax (list (sym (slice (find "(" line) -1 line))))
					(setq state :x)))
            (when (and (eql state :x) (> (length line) 9))
                (defq _ (elem 0 (split line (const (cat (ascii-char 9) " ()'" (ascii-char 13))))))
                (cond
                    ((or (eql _ "defun") (eql _ "defmacro") (eql _ "defclass")
							(eql _ "defmethod") (eql _ "deffimethod") (eql _ "defabstractmethod"))
                        (setq state :y))))) (file-stream file)))
        (cat (all-files "." "lisp.inc")
			'("class/lisp/boot.inc" "lib/anaphoric/anaphoric.inc" "lib/debug/debug.inc"
			 "lib/debug/profile.inc" "lib/collections/xmap.inc" "lib/collections/xset.inc"
			  "lib/collections/emap.inc" "lib/collections/collections.inc")))
    (sort cmp syntax)
    (defq stream (file-stream "docs/SYNTAX.md" file_open_write))
    (write-line stream (const (str "# Syntax" (ascii-char 10))))
    (each (lambda (line)
        (defq form (elem 0 (defq body (split line (const (cat (ascii-char 9) " )"))))))
		(cond
			((and (starts-with "(." form) (eql (elem 1 body) "this"))
				(write-line stream (cat "## " (slice 1 -1 form) (ascii-char 10)))
				(write-line stream (cat line (ascii-char 10))))
			((starts-with "(." form)
				(write-line stream (cat "## " (cat (elem 1 body) " " (elem 2 body)) (ascii-char 10)))
				(write-line stream (cat line (ascii-char 10))))
			((starts-with "(" form)
				(write-line stream (cat "## " (slice 1 -1 form) (ascii-char 10)))
				(write-line stream (cat line (ascii-char 10)))))) syntax)
    (print "-> docs/SYNTAX.md"))

(defq usage `(
(("-h" "--help")
"Usage: make [options] [all] [boot] [platforms] [doc] [syms] [it]
    options:
        -h --help: this help info.
    all: include all .vp files.
    boot: create a boot image.
    platforms: for all platforms not just the host.
    docs: scan source files and create documentation.
    syms: scan source files and create VP sys/symbols.inc.
    it: all of the above !")
))

(defun main ()
    ;initialize pipe details and command args, abort on error
    (when (and
            (defq stdio (create-stdio))
            (defq args (options stdio usage)))
        (defq args (map sym args) all (find-rev 'all args) boot (find-rev 'boot args) platforms (find-rev 'platforms args)
            docs (find-rev 'docs args) syms (find-rev 'syms args) it (find-rev 'it args))
        (cond
            (it (make-syms) (make-docs) (remake-all-platforms))
            ((and boot all platforms) (remake-all-platforms))
            ((and boot all) (remake-all))
            ((and boot platforms) (remake-platforms))
            ((and all platforms) (make-all-platforms))
            (all (make-all))
            (platforms (make-platforms))
            (boot (remake))
            (docs (make-docs))
            (syms (make-syms))
            (t (make)))))
