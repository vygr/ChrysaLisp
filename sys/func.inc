(include "sys/pii/class.inc")
(include "sys/task/class.inc")
(include "lib/asm/vp.inc")
(include "lib/trans/vp.inc")

;;;;;;;;;;;;;;;;;
; func structures
;;;;;;;;;;;;;;;;;

(def-struct fn_header ln_fnode_size
	(ushort length)
	(ushort entry)
	(ushort links)
	(ushort paths)
	(ushort stack)
	(offset pathname))

;;;;;;;;;;;
; functions
;;;;;;;;;;;

(defcvar '*func_name* nil '*strings* (list) '*paths* (list) '*links* (list)
	'*func_align* nil '*func_syms* (list))

(defun link-sym (_) (str "rl_" _))
(defun path-sym (_) (str "rp_" _))
(defun string-sym (_) (str "rs_" _))

(defun def-func (*name* &optional falign fstack)
	(defcvar '*emit_list* (clear '()))
	(setq *func_name* *name* *switch_nxt* 0 *func_align* (opt falign stack_align))
	(clear *strings* *paths* *links* *func_syms*)
(vp-label 'fn_start)
	(vp-long -1)
	(vp-short
		(label-sym 'fn_end)
		(label-sym 'fn_entry)
		(label-sym 'fn_links)
		(label-sym 'fn_paths)
		(opt fstack tk_stack_size))
(vp-label 'fn_name_start)
	(vp-cstr (str *name*))
	(vp-byte (list '- (label-sym 'fn_entry) (label-sym 'fn_name_start)))
	(vp-align +ptr_size (list '- (label-sym 'fn_entry) (label-sym 'fn_name_start)))
(vp-label 'fn_entry)
	(push-scope)
	(defcvar '*emit_start* (length *emit_list*))
	(env-push))

(defun def-func-end ()
	(env-pop)
	(defcvar '*emit_end* (length *emit_list*))
	(pop-scope-checked)
	(each (lambda (s)
	(vp-label (string-sym _))
		(vp-cstr s)) *strings*)
	(vp-align +ptr_size)
(vp-label 'fn_links)
	(each (lambda (s)
	(vp-label (link-sym _))
		(vp-long (list '- (label-sym (path-sym s)) '*pc*))) *links*)
(vp-label 'fn_paths)
	(each (lambda (s)
	(vp-label (path-sym _))
		(vp-cstr (str s))) *paths*)
	(vp-align +ptr_size)
(vp-label 'fn_end)
	(opt-emit-list *emit_list* *emit_start* *emit_end*)
	(when *debug_emit*
		(each emit-print *emit_list*))
	(defq out (emit-translate (emit-vp-code *emit_list*)))
	(if (> (length *func_syms*) 0)
		(apply undef (cat (list *compile_env*) *func_syms*)))
	(save out (cat "obj/" *cpu* "/" *abi* "/" (str *func_name*)))
	(print "-> " (str "obj/" *cpu* "/" *abi* "/" (str *func_name*) " (" (length out) ")")))

(defun fn-add-string (s)
	(defq i -1)
	(while (and (< (setq i (inc i)) (length *strings*)) (not (eql s (elem i *strings*)))))
	(if (= i (length *strings*)) (push *strings* s)) i)

(defun fn-add-path (p)
	(if (defq i (find-rev p *paths*)) i
		(dec (length (push *paths* p)))))

(defun fn-add-link (p)
	(push *links* (fn-add-path p)))

(defun fn-find-link (p)
	(defq i -1)
	(while (and (< (setq i (inc i)) (length *links*)) (not (eql p (elem (elem i *links*) *paths*)))))
	(if (= i (length *links*)) (fn-add-link p)) i)

(defun fn-string (s r)
	(vp-lea-p (string-sym (fn-add-string s)) r))

(defun fn-bind (p r)
	(if (get p) (throw "Can't bind to inline function !" p)
		(vp-cpy-pr (link-sym (fn-find-link p)) r)))

(defun fn-call (p)
	(if (get p) ((eval p))
		(vp-call-p (link-sym (fn-find-link p)))))

(defun fn-jump (p)
	(if (get p) (throw "Can't jump to inline function !" p)
		(vp-jmp-p (link-sym (fn-find-link p)))))

(defun abort (&optional s)
	(call 'sys_pii :write_str (list 2 (opt s "Abort !")))
	(jump 'sys_pii :exit '(1)))

(defun assert (b &optional d)
	(when (> *debug_mode* 0)
		(vpifnot b)
			(abort (opt d "Assert Failure !"))
		(endif)))
