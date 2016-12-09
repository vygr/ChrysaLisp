(import 'inc/vp.inc)
(import 'inc/code.inc)
(import 'inc/list.inc)
(import 'inc/syscall.inc)
(import 'inc/task.inc)

(defcvar 'debug_mode nil)
(defcvar 'debug_emit nil)

;;;;;;;;;;;;;;;;;;;;;;
; kernel mail funcions
;;;;;;;;;;;;;;;;;;;;;;

(def-enum)
	(enum 'kn_call_callback)
	(enum 'kn_call_task_open)
	(enum 'kn_call_task_child)
	(enum 'kn_call_task_route)
(def-enum-end)

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

(def-struct 'kn_msg 'msg_header)
	(ulong 'kn_msg_user)
	(struct 'kn_msg_reply_id 'id)
	(offset 'kn_msg_reply_size)
	(ulong 'kn_msg_function)
(def-struct-end)

(def-struct 'kn_msg_callback 'kn_msg)
	(ptr 'kn_msg_callback_addr)
(def-struct-end)

(def-struct 'kn_msg_open 'kn_msg)
	(offset 'kn_msg_open_pathname)
(def-struct-end)

(def-struct 'kn_msg_child 'kn_msg)
	(offset 'kn_msg_child_pathname)
(def-struct-end)

(def-struct 'kn_msg_link_route 'kn_msg)
	(ulong 'kn_msg_link_route_origin)
	(ulong 'kn_msg_link_route_via)
	(ulong 'kn_msg_link_route_hops)
(def-struct-end)

;;;;;;;;;;;;;;;;;
; func structures
;;;;;;;;;;;;;;;;;

(def-struct 'fn_header 'ln_fnode)
	(uint 'fn_header_length)
	(uint 'fn_header_entry)
	(uint 'fn_header_links)
	(uint 'fn_header_paths)
	(uint 'fn_header_stack)
	(offset 'fn_header_pathname)
(def-struct-end)

;;;;;;;;;;;
; functions
;;;;;;;;;;;

(defcvar '*func-name* nil '*strings* nil '*paths* nil '*links* nil)

(defcfun def-func (*name* &optional *func-stack*)
	(print "Compiling -> " *name*)
	(setq *func-name* *name* *switch-nxt* 0 *emit-buffer* (list 'progn))
	(setq *strings* (list) *paths* (list) *links* (list) *distance* (list))
	(vp-label '_func_start)
	(vp-long -1)
	(vp-int `(sub ,(label-sym '_func_end) ,(label-sym '_func_start))
		`(sub ,(label-sym '_func_entry) ,(label-sym '_func_start))
		`(sub ,(label-sym '_func_links) ,(label-sym '_func_start))
		`(sub ,(label-sym '_func_paths) ,(label-sym '_func_start))
		(if *func-stack* *func-stack* tk_stack_size))
	(vp-label '_func_name_start)
	(vp-string (str *name*))
	(vp-byte 0 `(sub ,(label-sym '_func_entry) ,(label-sym '_func_name_start)))
	(vp-align ptr_size `(sub ,(label-sym '_func_entry) ,(label-sym '_func_name_start)))
	(vp-label '_func_entry)
	(push-scope)
	(operator "." 1 0 compile-field)
	(operator "->" 1 0 compile-member)
	(operator "[]" 1 0 compile-index)
	(operator ":" 2 1 compile-uaddrof)
	(operator "_" 2 1 compile-uminus)
	(operator "#" 2 1 compile-uderef)
	(operator "~" 2 1 compile-unot)
	(operator "!" 2 1 compile-ulnot)
	(operator "*" 3 0 compile-mul)
	(operator "/" 3 0 compile-divu)
	(operator "%" 3 0 compile-remu)
	(operator "//" 3 0 compile-div)
	(operator "%%" 3 0 compile-rem)
	(operator "+" 4 0 compile-plus)
	(operator "-" 4 0 compile-minus)
	(operator "<<" 5 0 compile-lshift)
	(operator ">>" 5 0 compile-rshift)
	(operator ">>>" 5 0 compile-arshift)
	(operator "<" 6 0 compile-branch)
	(operator ">" 6 0 compile-branch)
	(operator "<=" 6 0 compile-branch)
	(operator ">=" 6 0 compile-branch)
	(operator "==" 7 0 compile-branch)
	(operator "!=" 7 0 compile-branch)
	(operator "&" 8 0 compile-and)
	(operator "^" 9 0 compile-xor)
	(operator "|" 10 0 compile-or)
	(operator "&&" 11 0 compile-land)
	(operator "||" 12 0 compile-lor)
	(operator "?" 13 0 compile-ternary)
	(operator "(" 14)
	(operator ")" 14)
	(operator "[" 14)
	(operator "]" 14)
	(push-scope))

(defcfun def-func-end ()
	(pop-scope)
	(pop-scope)
	(defq *cnt* 0)
	(each (lambda (s)
		(vp-label (cat "_ref_string_" (str *cnt*)))
		(vp-string s) (vp-byte 0)
		(setq *cnt* (inc *cnt*))) *strings*)
	(vp-align ptr_size)
	(vp-label '_func_links)
	(setq *cnt* 0)
	(each (lambda (i)
		(vp-label (cat "_ref_link_" (str *cnt*)))
		(vp-long `(sub ,(label-sym (cat "_ref_path_" (str i))) *pc*))
		(setq *cnt* (inc *cnt*))) *links*)
	(vp-long 0)
	(vp-label '_func_paths)
	(setq *cnt* 0)
	(each (lambda (s)
		(vp-label (cat "_ref_path_" (str *cnt*)))
		(vp-string (str s)) (vp-byte 0)
		(setq *cnt* (inc *cnt*))) *paths*)
	(vp-align ptr_size)
	(vp-label '_func_end)
	(when debug_emit
		(print-emit-buffer))
	(emit-passes)
	(print "Writing -> " *func-name*)
	(save *out-buffer* (cat "tst/" (str *func-name*))))

(defmacro def-insert (n l)
	`(defcfun ,n (s)
		(defq i 0)
		(while (and (lt i (length ,l)) (not (eql s (elem i ,l))))
			(setq i (inc i)))
		(if (eq i (length ,l)) (push ,l s))
		i))

(def-insert fn-add-string *strings*)
(def-insert fn-add-path *paths*)

(defcfun fn-string (s r)
	(vp-lea-p (cat "_ref_string_" (str (fn-add-string s))) r))

(defcfun fn-add-link (p)
	(push *links* (fn-add-path p)))

(defcfun fn-find-link (p)
	(defq i 0)
	(while (and (lt i (length *links*)) (not (eql p (elem (elem i *links*) *paths*))))
		(setq i (inc i)))
	(if (eq i (length *links*)) (fn-add-link p))
	i)

(defcfun fn-bind (p r)
	(vp-cpy-pr (cat "_ref_link_" (str (fn-find-link p))) r))

(defcfun fn-call (p)
	(vp-call-p (cat "_ref_link_" (str (fn-find-link p)))))

(defcfun fn-jmp (p)
	(vp-jmp-p (cat "_ref_link_" (str (fn-find-link p)))))

(defcfun debug-long (s i)
	;s = debug string
	;i = debug long
	(when debug_mode
		(vp-push r0 r1 r2 r3 r5)
		(vp-cpy-cr i r0)
		(vp-push r0)
		(vp-lea-p '_func_pathname) r0)
		(vp-cpy-cr 0 r1)
		(fn-string s r2)
		(vp-pop r3)
		(f-call 'sys_io 'debug_long '(r0 r1 r2 r3))
		(vp-pop r0 r1 r2 r3 r5))

(defcfun debug-str (s &optional d)
	;s = debug string
	;d = debug string
	(when debug_mode
		(vp-push r0 r1 r2 r3 r5)
		(vp-cpy-cr "" r0)
		(vp-push r0)
		(vp-lea-p '_func_pathname) r0)
		(vp-cpy-cr 0 r1)
		(fn-string s r2)
		(vp-pop r3)
		(f-call 'sys_io 'debug_str '(r0 r1 r2 r3))
		(vp-pop r0 r1 r2 r3 r5))

(defcfun abort (&optional s)
	(setq s (if s s "Abort !"))
	(debug-str s)
	(sys-exit 1))

(defcfun assert (b &optional d)
	;a = val1
	;b = condition
	;c = val2
	;d = debug string
	(when debug_mode
		(setq d (if d d "Assert Failure !"))
		(vpifnot b)
			(abort d)
		(endif)))
