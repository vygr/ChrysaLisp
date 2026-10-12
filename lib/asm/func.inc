(include "./code.inc")
(include "./scopes.inc")
(include "./vp.inc")
(include "./vpopt.inc")
(include "./vpx.inc")
(include "./vpxopt.inc")
(include "./vpxreg.inc")
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
		*links* (clear '()) *emit_list* (clear '()) *locals* (list) *func_regs* :nil *kept* :nil
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
	(env-push)
	(validatecase
		(and (not (ends-with "/vtable" (str *func_name*)))
			(not (starts-with "sys/" (str *func_name*)))
			(call :sys_task :stack))))

(defun fn-callee (inst)
	; (fn-callee inst) -> :nil | (trashes takes_and_gives)
	;what a call or a jump out of the function is of, for (keep-locals):
	;the registers its method is declared to trash, :nil if it does not
	;say, and those it takes and gives values in. :nil if it is not of a
	;method that is known: one bound to its function is, through its
	;link, and a virtual one has its class and name with it
	(case (first inst)
		((emit-call-p emit-jmp-p)
			(and (starts-with "_l_rl_" (defq label (str (second inst))))
				(defq link (str-as-num (slice label 6 -1))) (< -1 link (length *links*))
				(defq m (path-method (elem-get *paths* (elem-get *links* link))))
				;bound to the function, it trashes what the function does. Only
				;a call through the vtable may be of any class, and trash what
				;any of them does, Chris: "If it's called as a direct call ...
				;it will trash only what it does, if :vcall then you assume
				;it's the wider trashes"
				(list (ifn (third m) (second m)) (first m))))
		((emit-call-i emit-jmp-i)
			(and (> (length inst) 4) (defq class (elem-get inst 3)) (defq name (elem-get inst 4))
				(nql class :nil) (nql name :nil)
				(defq m (method-lookup class name))
				(list (method-trashes class name)
					(reduce (# (if (list? %1) (cat %0 (filter (const sym?) %1)) %0)) (list (third m) (elem-get m 3)) (list)))))))

(defun fn-keep-locals ()
	;the function asked, (vpx-regs): its named locals are kept in
	;registers where they can be. Only if it is a method that says what
	;its function trashes: a local's register is one of those, so that
	;what calls it sees none changed that was not. What a virtual method
	;is declared to trash is what any class called so may, more than the
	;one function does, and a caller bound to the function goes by the
	;function: so it is what is declared of the function, own, that is used
	(when (and *func_regs* (nempty? *locals*)
			(defq me (path-method *func_name*)) (third me)
			(defq got (keep-locals (slice *emit_list* *emit_start* -1) *locals*
				(filter (# (starts-with ":r" %0)) (third me)) (first me) (const fn-callee))))
		(defq head (slice *emit_list* 0 *emit_start*))
		(clear *emit_list*)
		(each (# (push *emit_list* %0)) head)
		(each (# (push *emit_list* %0)) (first got))
		(setq *kept* (second got))))

(defun def-func-end ()
	(env-pop)
	(undef *compile_env* '*func_env*)
	(fn-keep-locals)
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
		;the instructions, and after them, where what reads only the one
		;form does not see it, what the frames of named locals were
		(defq *out* (cat "(" (join (map (const str) *emit_code*) "\n") ")"
			(if (nempty? *locals*) (cat "\n" (str (cat '(locals) *locals*))) "")
			;and which of them were kept in registers, (vpx-regs)
			(if *kept* (cat "\n" (str (cat '(kept) *kept*))) "")))
		(if (eql *cpu* 'vp64) (save *out* (cat "obj/vp/" (str *func_name*))))
		(if *build_emit* (print *out*)))
	(defq *out* (emit-translate *emit_code*))
	;the header has 16 bits for the length, and for where the links and paths are
	(if (> (length *out*) 0xffff) (throw "Function too big, over 64KB !" *func_name*))
	(save *out* (cat "obj/" *cpu* "/" *abi* "/" (str *func_name*)))
	(if (> *build_verb* 0)
		(print "-> " (str "obj/" *cpu* "/" *abi* "/" (str *func_name*) " (" (length *out*) ")"))))

(defun abort (&optional s)
	;no ret as that screws up the tracer !
	;and we never come back anyways !
	(abi-push-trashed :r1 :r13 :r14)
	(call :host_os :pii_write_str (list 2 (ifn s "Abort !")))
	(call :host_os :pii_exit '(1))
	(abi-pop-trashed :r1 :r13 :r14))

(defun assert (b &optional d)
	(when (> *build_mode* 0)
		(vpifnot b)
			(abort (ifn d "Assert Failure !"))
		(endif)))
