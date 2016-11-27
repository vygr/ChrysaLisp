(import 'inc/vp.inc)
(import 'inc/code.inc)
(import 'inc/list.inc)
(import 'inc/syscall.inc)
(import 'inc/task.inc)

(equate 'debug_mode nil)

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

(defcompilefun def-func (*func-name* &optional *func-stack*)
	(setq *emit-buffer* (list) *out-buffer* (list))
	(setq *strings* (list) *paths* (list) *links* (list))
	(vp-label '_func_start)
	(vp-long -1)
	(vp-int '(sub _func_end _func_start)
		'(sub _func_entry _func_start)
		'(sub _func_links _func_start)
		'(sub _func_paths _func_start)
		(if *func-stack* *func-stack* tk_stack_size))
	(vp-label '_func_name_start)
	(vp-string (str *func-name*))
	(vp-byte 0 '(sub _func_entry _func_name_start))
	(vp-align ptr_size '(sub _func_entry _func_name_start))
	(vp-label '_func_entry))

(defcompilefun def-func-end ()
	(defq *cnt* 0)
	(each (lambda (s)
		(vp-label (sym (cat "_ref_" (str *cnt*) "_string")))
		(vp-string s) (vp-byte 0)
		(setq *cnt* (inc *cnt*))) *strings*)
	(vp-align ptr_size)
	(vp-label '_func_links)
	(setq *cnt* 0)
	(each (lambda (i)
		(vp-label (sym (cat "_ref_" (str *cnt*) "_link")))
		(vp-long `(sub ,(sym (cat "_ref_" (str i) "_path")) (length *out-buffer*)))
		(setq *cnt* (inc *cnt*))) *links*)
	(vp-long 0)
	(vp-label '_func_paths)
	(setq *cnt* 0)
	(each (lambda (s)
		(vp-label (sym (cat "_ref_" (str *cnt*) "_path")))
		(vp-string (str s)) (vp-byte 0)
		(setq *cnt* (inc *cnt*))) *paths*)
	(vp-align ptr_size)
	(vp-label '_func_end)
;	(print-emit-buffer)
	(emit-passes)
;	(print-out-buffer 16)
	(save *out-buffer* 'mytest))

(defmacro def-insert (n l)
	`(defcompilefun ,n (s)
		(defq i 0)
		(while (and (lt i (length ,l)) (not (eql s (elem i ,l))))
			(setq i (inc i)))
		(if (eq i (length ,l)) (push ,l s))
		i))

(def-insert fn-add-string *strings*)
(def-insert fn-add-path *paths*)

(defcompilefun fn-string (s r)
	(vp-lea-p (sym (cat "_ref_" (str (fn-add-string s)) "_string")) r))

(defcompilefun fn-add-link (p)
	(push *links* (fn-add-path p)))

(defcompilefun fn-find-link (p)
	(defq i 0)
	(while (and (lt i (length *links*)) (not (eql p (elem (elem i *links*) *paths*))))
		(setq i (inc i)))
	(if (eq i (length *links*)) (fn-add-link p))
	i)

(defcompilefun fn-bind (p r)
	(vp-cpy-pr (sym (cat "_ref_" (str (fn-find-link p)) "_link")) r))

(defcompilefun fn-call (p)
	(vp-call-p (sym (cat "_ref_" (str (fn-find-link p)) "_link"))))

(defcompilefun fn-jmp (p)
	(vp-jmp-p (sym (cat "_ref_" (str (fn-find-link p)) "_link"))))

(defcompilefun debug-long (s i)
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

(defcompilefun debug-str (s &optional d)
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

(defcompilefun abort (&optional s)
	(setq s (if s s "Abort !"))
	(debug-str s)
	(sys-exit 1))

(defcompilefun assert (b &optional d)
	;a = val1
	;b = condition
	;c = val2
	;d = debug string
	(when debug_mode
		(setq d (if d d "Assert Failure !"))
		(vpifnot b)
			(abort d)
		(endif)))
