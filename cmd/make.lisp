;imports
(import 'lib/asm/asm.inc)
(import 'lib/pipe/pipe.inc)
(import 'lib/options/options.inc)

(defmacro sfind (ss slst)
  `(some (lambda (_tvar)
    (if (eql ,ss _tvar) _)) ,slst))

(defun-bind make-tree (dir ext)
    (defq dirs (list) files (list))
    (each! 0 -1 (lambda (f d)
        (unless (starts-with "." f)
            (cond
                ((eql "4" d)
                    (push dirs (cat dir "/" f)))
                ((ends-with ext f)
                    (push files (cat dir "/" f))))))
        (unzip (split (pii-dirlist dir) ",") (list (list) (list))))
    (each (lambda (d)
        (setq files (cat files (make-tree d ext)))) dirs)
    files)

(defun-bind make-syms ()
	(print "Scanning source files...")
	(defq _syms_ '(
		argb_black argb_blue argb_cyan argb_green argb_grey1 argb_grey10 argb_grey11
		argb_grey12 argb_grey13 argb_grey14 argb_grey15 argb_grey2 argb_grey3
		argb_grey4 argb_grey5 argb_grey6 argb_grey7 argb_grey8 argb_grey9
		argb_magenta argb_red argb_white argb_yellow byte_size canvas_color
		canvas_flags cap_arrow cap_butt cap_round cap_square cap_tri component_id
		ev_msg_action_source_id ev_msg_key_key ev_msg_key_keycode
		ev_msg_mouse_buttons ev_msg_mouse_rx ev_msg_mouse_ry ev_msg_target_id
		ev_msg_type ev_type_gui ev_type_key ev_type_mouse flow_flag_align_hcenter
		flow_flag_align_hleft flow_flag_align_hright flow_flag_align_vbottom
		flow_flag_align_vcenter flow_flag_align_vtop flow_flag_down flow_flag_fillh
		flow_flag_fillw flow_flag_lasth flow_flag_lastw flow_flag_left
		flow_flag_right flow_flag_up in_mbox_id in_state int_size join_bevel
		join_miter join_round kn_call_child kn_call_open load_flag_film
		load_flag_noswap load_flag_shared long_size out_state ptr_size
		scroll_flag_horizontal scroll_flag_vertical short_size stdio_args
		stream_mail_state_started stream_mail_state_stopped
		stream_mail_state_stopping vdu_char_height vdu_char_width view_flag_at_back
		view_flag_dirty_all view_flag_opaque view_flag_solid view_flags view_h view_w
		view_x view_y file_open_read file_open_write)
		_vals_ (within-compile-env (lambda ()
			(each include (make-tree "." "class.inc"))
			(map eval _syms_)))
		stream (string-stream (cat "")))
	(write-line stream ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
	(write-line stream "; VP symbols, autogen do not edit !")
	(write-line stream ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
	(write-line stream "(defq")
	(each (lambda (s v)
		(write stream s)
		(write-char stream (ascii-code " "))
		(write-line stream (str v))) _syms_ _vals_)
	(write-line stream ")")
	(save (str stream) 'sys/symbols.inc)
	(print "-> sys/symbols.inc") nil)

(defun-bind make-doc ()
    (defq *abi* (abi) *cpu* (cpu))
    (defun-bind trim-whitespace (_)
        (trim-start (trim-start _ (ascii-char 9)) " "))
    (defun-bind trim-parens (_)
        (sym (trim-start (trim-end _ ")") "(")))
    (defun-bind chop (_)
        (when (defq e (find-rev (char 0x22) _))
            (setq _ (slice 0 e _))
            (sym (slice (inc (find-rev (char 0x22) _)) -1 _))))
    (print "Scanning source files...")
    (defq *imports* (list 'make.inc) classes (list) functions (list)
        docs (list) syntax (list) state 'x)
    (within-compile-env (lambda ()
        (include 'sys/func.inc)
        (each include (all-class-files))
        (each-mergeable (lambda (_)
            (each-line (lambda (_)
                (setq _ (trim-end _ (ascii-char 13)))
                (defq l (trim-whitespace _))
                (when (eql state 'y)
                    (if (and (>= (length l) 1) (eql (elem 0 l) ";"))
                        (push (elem -2 docs) (slice 1 -1 l))
                        (setq state 'x)))
                (when (and (eql state 'x) (>= (length l) 10) (eql "(" (elem 0 l)))
                    (defq s (split l " ')") _ (elem 0 s))
                    (cond
                        ((eql _ "(include")
                            (merge-obj *imports* (list (sym (elem 1 s)))))
                        ((eql _ "(def-class")
                            (push classes (list (sym (elem 1 s))
                                (if (= 3 (length s)) (sym (elem 2 s)) 'null))))
                        ((eql _ "(dec-method")
                            (push (elem -2 classes) (list (sym (elem 1 s)) (sym (elem 2 s)))))
                        ((eql _ "(def-method")
                            (setq state 'y)
                            (push docs (list))
                            (push functions (f-path (sym (elem 1 s)) (sym (elem 2 s)))))
                        ((or (eql _ "(def-func") (eql _ "(defcfun") (eql _ "(defcfun-bind"))
                            (setq state 'y)
                            (push docs (list))
                            (push functions (sym (elem 1 s))))
                        ((and (or (eql _ "(call") (eql _ "(jump")) (eql (elem 2 s) ":repl_error"))
                            (if (setq l (chop l))
                                (merge-obj syntax (list l))))))) (file-stream _))) *imports*)))
    ;create classes docs
    (sort (lambda (x y)
        (cmp (elem 0 x) (elem 0 y))) classes)
    (defq stream (string-stream (cat "")))
    (write-line stream (const (str "# Classes" (ascii-char 10))))
    (each (lambda ((class super &rest methods))
        (write-line stream (cat "## " class (ascii-char 10)))
        (write-line stream (cat "Super Class: " super (ascii-char 10)))
        (each (lambda ((method function))
            (write-line stream (cat "### " class ":" method " -> " function (ascii-char 10)))
            (when (and (defq i (find-rev function functions))
                    (/= 0 (length (elem i docs))))
                (write-line stream "```lisp")
                (each (lambda (_)
                    (write-line stream _)) (elem i docs))
                (write-line stream (const (str "```" (ascii-char 10)))))) methods)) classes)
    (save (str stream) 'docs/CLASSES.md)
    (print "-> docs/CLASSES.md")

    ;create commands docs
    (defq target 'docs/COMMANDS.md)
    (defun-bind extract-cmd (el)
     (first (split (second (split el "/")) ".")))
    (defun-bind keep-extracts? (el)
      (not (sfind el '("oops"))))
    (defun-bind cmd-collector (acc el)
      (push acc (list (sym el) (str el " -h"))))
    (defun-bind wrap-block (content)
      (if (/= (length content) 0)
        (str (const (pad (ascii-char 10) 2))
             "```lisp" (ascii-char 10)
             content (ascii-char 10)
             "```" (ascii-char 10))
        ""))
    (defun-bind generate-cmd-help (lst)
      (defq _eat_chunk "")
      (defun _eat (_x)
        (setq _eat_chunk (cat _eat_chunk (wrap-block _x))))
      (each (lambda (el)
              (setq _eat_chunk (cat _eat_chunk (str "## " (first el) (const (ascii-char 10)))))
              (pipe-run (second el) _eat)) lst)
      (save _eat_chunk target))

    (generate-cmd-help (reduce
                cmd-collector
                (sort cmp
                      (filter keep-extracts?
                              (map extract-cmd (make-tree "cmd" ".lisp"))))
                (list)))
    (print "-> docs/COMMANDS.md")

    ;create lisp syntax docs
    (each (lambda (_)
        (each-line (lambda (_)
            (setq _ (trim-end _ (ascii-char 13)))
            (defq l (trim-whitespace _))
            (when (eql state 'y)
                (if (and (>= (length l) 1) (eql (elem 0 l) ";"))
                    (merge-obj syntax (list (sym (slice 1 -1 l))))
                    (setq state 'x)))
            (when (and (eql state 'x) (>= (length l) 10) (eql "(" (elem 0 l)))
                (defq s (split l " ") _ (elem 0 s))
                (cond
                    ((or (eql _ "(defun") (eql _ "(defmacro") (eql _ "(defun-bind") (eql _ "(defmacro-bind"))
                        (setq state 'y))))) (file-stream _)))
        (cat (make-tree "." "lisp.inc")
            '(lib/anaphoric/anaphoric.inc class/lisp/boot.inc class/lisp/debug.inc)))
    (sort cmp syntax)
    (defq stream (string-stream (cat "")))
    (write-line stream (const (str "# Syntax" (ascii-char 10))))
    (each (lambda (_)
        (defq s (split _ " ") form (trim-parens (elem 0 s)))
        (when (eql "(" (elem 0 (elem 0 s)))
            (write-line stream (cat "## " form (ascii-char 10)))
            (write-line stream (cat _ (ascii-char 10))))) syntax)
    (save (str stream) 'docs/SYNTAX.md)
    (print "-> docs/SYNTAX.md"))

(defq usage `(
(("-h" "--help")
"Usage: make [options] [all] [boot] [platforms] [doc] [syms]
    options:
        -h --help: this help info.
    all: include all .vp files.
    boot: create a boot image.
    platforms: for all platforms not just the host.
    docs: scan source files and create documentation.
    syms: scan source files and create VP sys/symbols.inc.")
))

(defun-bind main ()
    ;initialize pipe details and command args, abort on error
    (when (and
            (defq stdio (create-stdio))
            (defq args (options stdio usage)))
        (defq args (map sym args) all (find-rev 'all args) boot (find-rev 'boot args) platforms (find-rev 'platforms args)
            docs (find-rev 'docs args) syms (find-rev 'syms args))
        (cond
            ((and boot all platforms) (remake-all-platforms))
            ((and boot all) (remake-all))
            ((and boot platforms) (remake-platforms))
            ((and all platforms) (make-all-platforms))
            (all (make-all))
            (platforms (make-platforms))
            (boot (remake))
            (docs (make-doc))
            (syms (make-syms))
            (t (make)))))
