(import 'inc/code.inc)
(import 'inc/mail.inc)
(import 'inc/vp.inc)
(import 'inc/task.inc)
(import 'inc/syscall.inc)

(defcvar 'debug_mode t)
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

(defcfun link-sym (p)
	(scat "_ref_link_" (str p)))

(defcfun path-sym (p)
	(scat "_ref_path_" (str p)))

(defcfun string-sym (p)
	(scat "_ref_string_" (str p)))

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
	(push-scope))

(defcfun def-func-end ()
	(pop-scope)
	(each (lambda (s)
		(vp-label (string-sym _e))
		(vp-string s) (vp-byte 0)) *strings*)
	(vp-align ptr_size)
	(vp-label '_func_links)
	(each (lambda (i)
		(vp-label (link-sym _e))
		(vp-long `(sub ,(label-sym (path-sym i)) *pc*))) *links*)
	(vp-long 0)
	(vp-label '_func_paths)
	(each (lambda (s)
		(vp-label (path-sym _e))
		(vp-string (str s)) (vp-byte 0)) *paths*)
	(vp-align ptr_size)
	(vp-label '_func_end)
	(when debug_emit
		(print-emit-buffer))
	(emit-passes)
	(save *out-buffer* (cat "obj/" (str *func-name*))))

(defmacro def-insert (n l)
	`(defcfun ,n (s)
		(defq i -1)
		(while (and (lt (setq i (inc i)) (length ,l)) (not (eql s (elem i ,l)))))
		(if (eq i (length ,l)) (push ,l s))
		i))

(def-insert fn-add-string *strings*)
(def-insert fn-add-path *paths*)

(defcfun fn-string (s r)
	(vp-lea-p (string-sym (fn-add-string s)) r))

(defcfun fn-add-link (p)
	(push *links* (fn-add-path p)))

(defcfun fn-find-link (p)
	(defq i -1)
	(while (and (lt (setq i (inc i)) (length *links*)) (not (eql p (elem (elem i *links*) *paths*)))))
	(if (eq i (length *links*)) (fn-add-link p))
	i)

(defcfun fn-bind (p r)
	(vp-cpy-pr (link-sym (fn-find-link p)) r))

(defcfun fn-call (p)
	(vp-call-p (link-sym (fn-find-link p))))

(defcfun fn-jmp (p)
	(vp-jmp-p (link-sym (fn-find-link p))))

(defcfun debug-long (s i)
	;s = debug string
	;i = debug long
	(when debug_mode
		(vp-push r0 r1 r2 r3 r5)
		(vp-cpy-rr i r0)
		(vp-push r0)
		(vp-lea-p '_func_name_start r0)
		(vp-cpy-cr 0 r1)
		(fn-string s r2)
		(vp-pop r3)
		(f-call 'sys_io 'debug_long '(r0 r1 r2 r3))
		(vp-pop r0 r1 r2 r3 r5)))

(defcfun debug-str (s &optional d)
	;s = debug string
	;d = debug string
	(when debug_mode
		(vp-push r0 r1 r2 r3 r5)
		(fn-string (if d d "") r0)
		(vp-push r0)
		(vp-lea-p '_func_name_start r0)
		(vp-cpy-cr 0 r1)
		(fn-string s r2)
		(vp-pop r3)
		(f-call 'sys_io 'debug_str '(r0 r1 r2 r3))
		(vp-pop r0 r1 r2 r3 r5)))

(defcfun abort (&optional s)
	(setq s (if s s "Abort !"))
	(debug-str s)
	(sys-exit '(1)))

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
