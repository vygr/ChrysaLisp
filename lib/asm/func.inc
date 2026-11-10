(include "./code.inc")
(include "./vp.inc")
(include "./vpopt.inc")
(include "./assign.inc")
(include "./lisp.inc")
(include "././trans/vp.inc")

(include "sys/pii/class.inc")
(include "sys/task/class.inc")
(include "sys/statics/class.inc")

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
	(vp-lea-pr (string-sym (fn-add-string s)) r))

(defun fn-bind (p r)
	(if (get p) (throw "can't bind to inline function !" p)
		(vp-load-pr (link-sym (fn-find-link p)) r)))

(defun fn-call (p)
	(if (get p) ((eval p))
		(vp-call-p (link-sym (fn-find-link p)))))

(defun fn-jump (p)
	(if (get p) (throw "can't jump to inline function !" p)
		(vp-jmp-p (link-sym (fn-find-link p)))))

(defun def-func (*func_name* &optional *func_stack*)
	(defq *strings* (clear '()) *paths* (clear '()) *consts* (clear '())
		*links* (clear '()) *emit_list* (clear '()) *locals* (list)
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
	(defq *emit_start* (length *emit_list*))
	(def *compile_env* '*func_env* (env))
	(env-push)
(validatecase
	(and (not (ends-with "/vtable" (str *func_name*)))
		(not (starts-with "sys/" (str *func_name*)))
		(call :sys_task :stack))))

(defun def-func-end ()
	(env-pop)
	(undef *compile_env* '*func_env*)
	(defq *emit_end* (length *emit_list*))
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
	;the function's VP, the same for every platform, all a compile gives:
	;its name, the instructions, and after them what the frames of named
	;locals were. A copy, the list (emit-vp-code) gives is its own and
	;made again for the next
	(unless *build_entries* (throw "VP compiled other than by (vp-compile) !" *func_name*))
	(defq entry (list (str *func_name*) (cat (emit-vp-code *emit_list*))))
	(if (nempty? *locals*) (push entry (cat '(locals) *locals*)))
	(push *build_entries* entry)
	(if *build_emit* (print (vp-code-text entry))))

(defun abort (&optional s)
	;no ret as that screws up the tracer !
	;and we never come back anyways !
	(vp-push :r1 :r13 :r14)
	(call :host_os :pii_write_str (list 2 (ifn s "Abort !")))
	(call :host_os :pii_exit '(1))
	(vp-pop :r1 :r13 :r14))

(defun assert (b &optional d)
	(when (> *build_mode* 0)
		(vpifnot b)
			(abort (ifn d "Assert Failure !"))
		(endif)))
