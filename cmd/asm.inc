;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)

;C++ ChrysaLisp ?
(unless (def? 'nums)
	(defun kernel-total () 1))

(defun-bind compile (files &optional *abi* *cpu* num_child)
	(setd *abi* (abi) *cpu* (cpu) num_child (max 1 (/ (* (kernel-total) 80) 100)))
	(unless (lst? files)
		(setq files (list files)))
	(setq files (shuffle (map sym files)))
	(if (def? 'nums)
		;Native ChrysaLisp
		(progn
			(defq data_in (list) select (array) num_child (min num_child (length files))
				child_mboxs (open-farm "class/lisp/compile.lisp" num_child kn_call_child)
				buckets (map (lambda (_) (list)) child_mboxs) err (list))
			(each (lambda (file)
				(push (elem (% _ num_child) buckets) file)) files)
			(each (lambda (_)
				(push select (defq inbox (in-mbox (elem -2 (push data_in (in-stream))))))
				(mail-send (str (list (pop buckets) inbox *abi* *cpu* *debug_mode* *debug_emit* *debug_inst*)) _)) child_mboxs)
			(while (/= (length select) 0)
				;read all child data
				(each-line (lambda (data)
					(if (starts-with "Error:" data)
						(push err data)
						(print data))) (elem (defq idx (mail-select select)) data_in))
				;child closed
				(setq data_in (erase data_in idx (inc idx)) select (erase select idx (inc idx))))
			(each print err))
		;C++ ChrysaLisp !
		(within-compile-env (lambda ()
			(each include files))))
	(print "Done") nil)

;;;;;;;;;;;;;
; make system
;;;;;;;;;;;;;

(defun-bind make-imports (_)
	;create list of immediate dependencies
	(defq d (list 'cmd/asm.inc 'class/lisp/boot.inc _))
	(each-line (lambda (_)
		(when (starts-with "(include" _)
			(push d (sym (trim-start (trim-end
				(elem 1 (split (trim-end _ (ascii-char 13)) " ")) ")") "'"))))) (file-stream _)) d)

(defun-bind make-info (_)
	;create lists of immediate dependencies and products
	(defq d (list 'cmd/asm.inc 'class/lisp/boot.inc _) p (list))
	(defun-bind trim-cruft (_)
		(sym (trim-start (trim-end _ ")") "'")))
	(each-line (lambda (_)
		(when (and (>= (length _) 10) (eql "(" (elem 0 _))
				(<= 2 (length (defq s (split (trim-end _ (ascii-char 13)) " "))) 4))
			(defq _ (elem 0 s))
			(cond
				((eql _ "(def-method")
					(push p (f-path (trim-cruft (elem 1 s)) (trim-cruft (elem 2 s)))))
				((eql _ "(include")
					(push d (trim-cruft (elem 1 s))))
				((eql _ "(gen-vtable")
					(push p (f-path (trim-cruft (elem 1 s)) 'vtable)))
				((eql _ "(gen-create")
					(push p (f-path (trim-cruft (elem 1 s))
						(if (> (length s) 2) (sym (cat "create_" (trim-cruft (elem 2 s)))) 'create))))
				((eql _ "(def-func")
					(push p (trim-cruft (elem 1 s))))))) (file-stream _))
	(list d p))

(defun-bind func-obj (_)
	(cat "obj/" *cpu* "/" *abi* "/" _))

(defun-bind make (&optional root *abi* *cpu*)
	(setd root 'make.inc *abi* (abi) *cpu* (cpu))
	(compile ((lambda ()
		(defq *env* (env 101) *imports* (list root))
		(catch (progn
			(defun-bind make-sym (_)
				(sym (cat "_dep_" _)))
			(defun-bind make-time (_)
				;modification time of a file, cached
				(defq s (sym (cat "_age_" _)))
				(or (def? s) (def *env* s (age _))))
			;list of all file imports while defining dependencies and products
			(within-compile-env (lambda ()
				(include 'sys/func.inc)
				(each include (all-class-files))
				(each-mergeable (lambda (_)
					(defq i (make-info _))
					(bind '(d p) i)
					(merge *imports* d)
					(elem-set 1 i (map func-obj p))
					(def *env* (make-sym _) i)) *imports*)))
			;filter to only the .vp files
			(setq *imports* (filter (lambda (_)
				(ends-with ".vp" _)) *imports*))
			;filter to only the files who's oldest product is older than any dependency
			(setq *imports* (filter (lambda (_)
				(defq d (eval (make-sym _)) p (reduce min (map make-time (elem 1 d))) d (elem 0 d))
				(each-mergeable (lambda (_)
					(merge d (elem 0 (eval (make-sym _))))) d)
				(some (lambda (_) (>= _ p)) (map make-time d))) *imports*))
			;drop the make environment, return the list to compile
			(setq *env* nil) *imports*) (setq *env* nil)))) *abi* *cpu*))

(defun-bind make-boot (&optional r *funcs* *abi* *cpu*)
	(within-compile-env (lambda ()
		(setd *funcs* (list) *abi* (abi) *cpu* (cpu))
		(defq *env* (env 31) z (cat (char 0 8) (char 0 4)))
		(catch (progn
			(include 'sys/func.inc)
			(defun-bind func-sym (_)
				(sym (cat 'f_ _)))
			(defun-bind read-paths (_)
				(defq l (list) i (get-short _ fn_header_links) e (get-short _ fn_header_paths))
				(while (/= i e)
					(push l (sym (get-cstr _ (+ (get-long _ i) i))))
					(setq i (+ i 8))) l)
			(defun-bind load-func (_)
				(or (def? (defq s (func-sym _)))
					(progn
						(unless (defq b (load (func-obj _)))
							(throw "No such file !" (func-obj _)))
						(defq h (slice fn_header_entry (defq l (get-short b fn_header_links)) b)
							l (slice l (defq p (get-short b fn_header_paths)) b))
						(def *env* s (list (cat (char -1 8) (char p 2) h) l (read-paths b))))))
			(unless (lst? *funcs*)
				(setq *funcs* (list *funcs*)))
			(defq f (list
				;must be first function !
				'sys/load/init
				;must be second function !
				'sys/load/bind
				;must be third function !
				'sys/load/statics))
			;load all loader dependents
			(each-mergeable (lambda (_)
				(merge f (elem 2 (load-func _)))) f)
			(defq fs (length f))
			;load up all extra functions requested
			(merge f (map sym *funcs*))
			(each load-func f)
			;if recursive then load up all dependents
			(when r
				(each-mergeable (lambda (_)
					(merge f (elem 2 (load-func _)))) f))
			;sort into order, leaving the loader dependents first !
			(sort cmp f fs)
			;list of all function bodies and links in order, list of offsets of header and link sections
			;and offset of new strings section
			(defq b (map (lambda (_) (eval (func-sym _))) f)
				ns (list) nso (list) ho (list) lo (list) so (+ (length z) (reduce (lambda (x y)
				(push ho x)
				(push lo (setq x (+ x (length (elem 0 y)))))
				(+ x (length (elem 1 y)))) b 0)))
			;list of all strings that will appear in new strings section, and list of all new string offsets
			(each (lambda (_)
				(each (lambda (_)
					(unless (find _ f) (merge ns (list _)))) (elem 2 (eval (func-sym _))))) f)
			(reduce (lambda (x y)
				(push nso x)
				(+ x (length y) 1)) ns 0)
			;create new link sections with offsets to header strings or new strings
			(each (lambda (x)
				(defq u (elem _ lo))
				(elem-set 1 x (apply cat (push (map (lambda (y)
					(char (- (if (defq i (find y f))
						(+ (elem i ho) fn_header_pathname)
						(+ (elem (find y ns) nso) so)) (+ u (* _ 8))) 8)) (elem 2 x)) "")))) b)
			;build list of all sections of boot image
			;concatenate all sections and save
			(save (setq f (apply cat (reduce (lambda (x y)
				(push x (cat y (ascii-char 0)))) ns (push (reduce (lambda (x y)
					(push x (elem 0 y) (elem 1 y))) b (list)) z)))) (func-obj 'sys/boot_image))
			(print "image -> " (func-obj 'sys/boot_image) " (" (length f) ")")
			(setq *env* nil)) (setq *env* nil)))))

(defun-bind make-boot-all (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make-boot nil
		(within-compile-env (lambda ()
			(include 'sys/func.inc)
			(each include (all-class-files))
			(defq *prods* (list))
			;lists of all file imports and products
			(each-mergeable (lambda (_)
				(merge *prods* (bind '(d _) (make-info _)))
				(merge _l d)) (list 'make.inc)) *prods*)) *abi* *cpu*))

(defun-bind all-vp-files ()
	;filter to only the .vp files
	(filter (lambda (_) (ends-with ".vp" _))
		;the list of all file imports
		(each-mergeable (lambda (_)
			(merge _l (make-imports _))) (list 'make.inc))))

(defun-bind all-class-files ()
	;filter to only the class.inc files
	(filter (lambda (_) (ends-with "class.inc" _))
		;the list of all file imports
		(each-mergeable (lambda (_)
			(merge _l (make-imports _))) (list 'make.inc))))

(defun-bind make-all (&optional *abi* *cpu* files)
	(setd *abi* (abi) *cpu* (cpu))
	(compile (opt files (all-vp-files)) *abi* *cpu*))

(defun-bind remake (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make 'make.inc *abi* *cpu*)
	(make-boot-all *abi* *cpu*))

(defun-bind remake-all (&optional *abi* *cpu* files)
	(setd *abi* (abi) *cpu* (cpu))
	(make-all *abi* *cpu* files)
	(make-boot-all *abi* *cpu*))

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
		view_x view_y)
		_vals_ (within-compile-env (lambda ()
			(each include
				'(sys/kernel/class.inc sys/list/class.inc sys/mail/class.inc class/in/class.inc
				class/out/class.inc class/stdio/class.inc gui/gui/class.inc gui/ctx/class.inc
				gui/flow/class.inc gui/vdu/class.inc gui/window/class.inc gui/scroll/class.inc
				gui/canvas/class.inc gui/path/class.inc))
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

;;;;;;;;;;;;;;;;;;;;;
; cross platform make
;;;;;;;;;;;;;;;;;;;;;

(defun-bind make-platforms ()
	(make 'make.inc 'AMD64 'x86_64)
	(make 'make.inc 'WIN64 'x86_64)
	(make 'make.inc 'ARM64 'aarch64))

(defun-bind make-all-platforms ()
	(defq files (all-vp-files))
	(make-all 'AMD64 'x86_64 files)
	(make-all 'WIN64 'x86_64 files)
	(make-all 'ARM64 'aarch64 files))

(defun-bind remake-platforms ()
	(remake 'AMD64 'x86_64)
	(remake 'WIN64 'x86_64)
	(remake 'ARM64 'aarch64))

(defun-bind remake-all-platforms ()
	(defq files (all-vp-files))
	(remake-all 'AMD64 'x86_64 files)
	(remake-all 'WIN64 'x86_64 files)
	(remake-all 'ARM64 'aarch64 files))

;;;;;;;;;;;;;;;;;;;;;;;;
; compile and make tests
;;;;;;;;;;;;;;;;;;;;;;;;

(defun-bind make-test (&optional i &optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(defun-bind time-in-seconds (_)
		(str (/ _ 1000000) "." (pad (% _ 1000000) 6 "00000")))
	(defq b 1000000000 w 0 a 0 c 0 files (all-vp-files))
	(times (opt i 10)
		(defq _ (time))
		(compile files *abi* *cpu*)
		(setq _ (- (time) _) a (+ a _) c (inc c))
		(print "Time " (time-in-seconds _) " seconds")
		(print "Mean time " (time-in-seconds (/ a c)) " seconds")
		(print "Best time " (time-in-seconds (setq b (min b _))) " seconds")
		(print "Worst time " (time-in-seconds (setq w (max w _))) " seconds"))
	nil)

(defun-bind compile-test (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(each (lambda (_)
		(compile _ *abi* *cpu*)) (defq f (all-vp-files)))
	(compile f *abi* *cpu* 1))
