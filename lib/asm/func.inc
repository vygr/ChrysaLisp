(include "./code.inc")
(include "./scopes.inc")
(include "./vp.inc")
(include "./vpopt.inc")
(include "./cscript.inc")
(include "./csopt.inc")
(include "./assign.inc")
(include "./lisp.inc")
(include "././trans/vp.inc")

(include "sys/pii/class.inc")
(include "sys/task/class.inc")

;;;;;;;;;;;
; functions
;;;;;;;;;;;

(defun link-sym (%0) (str "rl_" %0))
(defun path-sym (%0) (str "rp_" %0))
(defun string-sym (%0) (str "rs_" %0))

(defun fn-add-string (s)
	(find s (merge *strings* (list s))))

(defun fn-add-path (p)
	(find p (merge *paths* (list p))))

(defun fn-add-link (p)
	(push *links* (fn-add-path p)))

(defun fn-find-link (p)
	(if (defq i (some (# (if (eql p (elem-get *paths* %0)) (!))) *links*)) i
		(dec (length (fn-add-link p)))))

(defun fn-const (c)
	(* (find (defq c (logior c)) (merge *consts* (list c))) +long_size))

(defun fn-consts (&rest c)
	(map (const fn-const) c))

(defun fn-string (s r)
	(vp-lea-p (string-sym (fn-add-string s)) r))

(defun fn-bind (p r)
	(if (get p) (throw "can't bind to inline function !" p)
		(vp-cpy-pr (link-sym (fn-find-link p)) r)))

(defun fn-call (p)
	(if (get p) ((eval p))
		(vp-call-p (link-sym (fn-find-link p)))))

(defun fn-jump (p)
	(if (get p) (throw "can't jump to inline function !" p)
		(vp-jmp-p (link-sym (fn-find-link p)))))

(defun def-func (*func_name* &optional *func_align* *func_stack*)
	(defq *func_align* (ifn *func_align* stack_align)
		*strings* (clear '()) *paths* (clear '()) *consts* (clear '())
		*links* (clear '()) *emit_list* (clear '())
		*switch_stk* (clear '()) *switch* :nil *switch_nxt* 0)
(vp-label 'fn_start)
	(vp-long -1)
	(vp-short
		(label-sym 'fn_end)
		(label-sym 'fn_entry)
		(label-sym 'fn_links)
		(label-sym 'fn_paths)
		(ifn *func_stack* tk_stack_size))
(vp-label 'fn_name_start)
	(vp-cstr (str *func_name*))
	(vp-byte (list '- (label-sym 'fn_entry) (label-sym 'fn_name_start)))
	(vp-align +ptr_size (list '- (label-sym 'fn_entry) (label-sym 'fn_name_start)))
(vp-label 'fn_entry)
	(push-scope)
	(defq *emit_start* (length *emit_list*))
	(def *compile_env* '*func_env* (env))
	(env-push))

(defun def-func-end ()
	(env-pop)
	(undef *compile_env* '*func_env*)
	(defq *emit_end* (length *emit_list*))
	(pop-scope-checked)
	(each (# (vp-label (string-sym (!))) (vp-cstr %0)) *strings*)
	(cond
		((nempty? *consts*)
			(vp-align +long_size) (vp-label 'fn_consts) (each (const vp-long) *consts*))
		((vp-align +ptr_size)))
(vp-label 'fn_links)
	(each (# (vp-label (link-sym (!)))
		(vp-long (list '- (label-sym (path-sym %0)) '*pc*))) *links*)
(vp-label 'fn_paths)
	(when (nempty? *paths*)
		(each (# (vp-label (path-sym (!))) (vp-cstr (str %0))) *paths*)
		(vp-align +ptr_size))
(vp-label 'fn_end)
	(opt-emit-list *emit_list* *emit_start* *emit_end*)
	(defq *emit_code* (emit-vp-code *emit_list*))
	(when (or *build_emit* (eql *cpu* 'vp64))
		(defq *out* (cat "(" (join (map (const str) *emit_code*) (ascii-char 10)) ")"))
		(if (eql *cpu* 'vp64) (save *out* (cat "obj/vp/" (str *func_name*))))
		(if *build_emit* (print *out*)))
	(defq *out* (emit-translate *emit_code*))
	(save *out* (cat "obj/" *cpu* "/" *abi* "/" (str *func_name*)))
	(print "-> " (str "obj/" *cpu* "/" *abi* "/" (str *func_name*) " (" (length *out*) ")")))

(defun abort (&optional s)
	(call :host_os :pii_write_str (list 2 (ifn s "Abort !")))
	(jump :host_os :pii_exit '(1)))

(defun assert (b &optional d)
	(when (> *build_mode* 0)
		(vpifnot b)
			(abort (ifn d "Assert Failure !"))
		(endif)))
