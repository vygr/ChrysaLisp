(import 'sys/code.inc)
(import 'sys/vp.inc)
(import 'sys/pii/pii.inc)
(import 'sys/task.inc)

;;;;;;;;;;;;;;;;;
; func structures
;;;;;;;;;;;;;;;;;

(def-struct 'fn_header 'ln_fnode)
	(uint 'length)
	(uint 'entry)
	(uint 'links)
	(uint 'paths)
	(uint 'stack)
	(offset 'pathname)
(def-struct-end)

;;;;;;;;;;;
; functions
;;;;;;;;;;;

(defcvar '*func-name* nil '*strings* nil '*paths* nil '*links* nil)

(defcfun link-sym (p)
	(cat "_ref_link_" (str p)))

(defcfun path-sym (p)
	(cat "_ref_path_" (str p)))

(defcfun string-sym (p)
	(cat "_ref_string_" (str p)))

(defcfun def-func (*name* &optional *func-stack*)
	(print "-> " *name*)
	(emit-reset)
	(setq *func-name* *name* *switch-nxt* 0 *strings* (list) *paths* (list) *links* (list))
	(vp-label '_func_start)
	(vp-long -1)
	(vp-int `(sub ,(label-sym '_func_end) ,(label-sym '_func_start))
		`(sub ,(label-sym '_func_entry) ,(label-sym '_func_start))
		`(sub ,(label-sym '_func_links) ,(label-sym '_func_start))
		`(sub ,(label-sym '_func_paths) ,(label-sym '_func_start))
		(opt *func-stack* tk_stack_size))
	(vp-label '_func_name_start)
	(vp-string (str *name*))
	(vp-byte 0 `(sub ,(label-sym '_func_entry) ,(label-sym '_func_name_start)))
	(vp-align ptr_size `(sub ,(label-sym '_func_entry) ,(label-sym '_func_name_start)))
	(vp-label '_func_entry)
	(push-scope))

(defcfun def-func-end ()
	(pop-scope-checked)
	(each (lambda (s)
		(vp-label (string-sym _))
		(vp-string s) (vp-byte 0)) *strings*)
	(vp-align ptr_size)
	(vp-label '_func_links)
	(each (lambda (s)
		(vp-label (link-sym _))
		(vp-long `(sub ,(label-sym (path-sym s)) *pc*))) *links*)
	(vp-long 0)
	(vp-label '_func_paths)
	(each (lambda (s)
		(vp-label (path-sym _))
		(vp-string (str s)) (vp-byte 0)) *paths*)
	(vp-align ptr_size)
	(vp-label '_func_end)
	(opt-emit-buffer)
	(when debug_emit
		(each print-emit *emit-buffer*))
	(emit-passes)
	(save *out-buffer* (cat "obj/" (str *func-name*))))

(defcfun fn-add-string (s)
	(defq i -1)
	(while (and (lt (setq i (inc i)) (length *strings*)) (not (eql s (elem i *strings*)))))
	(if (eq i (length *strings*)) (push *strings* s)) i)

(defcfun fn-add-path (p)
	(if (defq i (find p *paths*)) i
		(dec (length (push *paths* p)))))

(defcfun fn-add-link (p)
	(push *links* (fn-add-path p)))

(defcfun fn-find-link (p)
	(defq i -1)
	(while (and (lt (setq i (inc i)) (length *links*)) (not (eql p (elem (elem i *links*) *paths*)))))
	(if (eq i (length *links*)) (fn-add-link p)) i)

(defcfun fn-string (s r)
	(vp-lea-p (string-sym (fn-add-string s)) r))

(defcfun fn-bind (p r)
	(if (def? p) (throw "Can't bind to inline function !" p)
		(vp-cpy-pr (link-sym (fn-find-link p)) r)))

(defcfun fn-call (p)
	(if (def? p) ((eval p))
		(vp-call-p (link-sym (fn-find-link p)))))

(defcfun fn-jmp (p)
	(if (def? p) (throw "Can't jump to inline function !" p)
		(vp-jmp-p (link-sym (fn-find-link p)))))

(defcfun debug-long (s i)
	;s = debug string
	;i = debug long
	(when debug_mode
		(vp-push r0 r1 r2 r3 r4)
		(vp-cpy-rr i r0)
		(vp-push r0)
		(fn-string (cat *stream-name* "<" (str *stream-line*) ">") r0)
		(fn-string s r1)
		(vp-pop r2)
		(f-call 'pii 'debug_long '(r0 r1 r2))
		(vp-pop r0 r1 r2 r3 r4)))

(defcfun debug-str (s &optional d)
	;s = debug string
	;d = debug string
	(when debug_mode
		(vp-push r0 r1 r2 r3 r4)
		(if d
			(vp-cpy-rr d r0)
			(fn-string "" r0))
		(vp-push r0)
		(fn-string (cat *stream-name* "<" (str *stream-line*) ">") r0)
		(fn-string s r1)
		(vp-pop r2)
		(f-call 'pii 'debug_str '(r0 r1 r2))
		(vp-pop r0 r1 r2 r3 r4)))

(defcfun abort (&optional s)
	(f-call 'pii 'write_str `(2 ,(opt s "Abort !")))
	(f-jmp 'pii 'exit '(1)))

(defcfun assert (b &optional d)
	;a = val1
	;b = condition
	;c = val2
	;d = debug string
	(when debug_mode
		(vpifnot b)
			(abort (opt d "Assert Failure !"))
		(endif)))
