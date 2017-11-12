(import 'sys/code.inc)
(import 'sys/vp.inc)
(import 'sys/pii/pii.inc)
(import 'sys/task/task.inc)

;;;;;;;;;;;;;;;;;
; func structures
;;;;;;;;;;;;;;;;;

(def-struct 'fn_header 'ln_fnode)
	(ushort 'length)
	(ushort 'entry)
	(ushort 'atoms)
	(ushort 'links)
	(ushort 'syms)
	(ushort 'paths)
	(ushort 'stack)
	(offset 'pathname)
(def-struct-end)

;;;;;;;;;;;
; functions
;;;;;;;;;;;

(defcvar '*func-name* nil '*strings* nil '*paths* nil '*links* nil '*atoms* nil '*strs* nil)

(defcfun link-sym (p)
	(cat "ref_link_" (str p)))

(defcfun path-sym (p)
	(cat "ref_path_" (str p)))

(defcfun atom-sym (p)
	(cat "ref_atom_" (str p)))

(defcfun string-sym (p)
	(cat "ref_string_" (str p)))

(defcfun str-sym (p)
	(cat "ref_str_" (str p)))

(defcfun def-func (*name* &optional *func-stack*)
	(print "-> " (cat "obj/" *os* "/" *cpu* "/" (str *name*)))
	(emit-reset)
	(setq *func-name* *name* *switch-nxt* 0 *strings* (list) *paths* (list)
		*links* (list) *atoms* (list) *strs* (list))
(vp-label 'func_start)
	(vp-long -1)
	(vp-short (list 'sub (label-sym 'func_end) (label-sym 'func_start))
		(list 'sub (label-sym 'func_entry) (label-sym 'func_start))
		(list 'sub (label-sym 'func_atoms) (label-sym 'func_start))
		(list 'sub (label-sym 'func_links) (label-sym 'func_start))
		(list 'sub (label-sym 'func_syms) (label-sym 'func_start))
		(list 'sub (label-sym 'func_paths) (label-sym 'func_start))
		(opt *func-stack* tk_stack_size))
(vp-label 'func_name_start)
	(vp-string (str *name*))
	(vp-byte 0 (list 'sub (label-sym 'func_entry) (label-sym 'func_name_start)))
	(vp-align ptr_size (list 'sub (label-sym 'func_entry) (label-sym 'func_name_start)))
(vp-label 'func_entry)
	(push-scope))

(defcfun def-func-end ()
	(pop-scope-checked)
	(each (lambda (s)
	(vp-label (string-sym _))
		(vp-string s) (vp-byte 0)) *strings*)
	(vp-align ptr_size)
(vp-label 'func_atoms)
	(each (lambda (s)
	(vp-label (atom-sym _))
		(vp-long (list 'sub (label-sym (str-sym s)) '*pc*))) *atoms*)
(vp-label 'func_links)
	(each (lambda (s)
	(vp-label (link-sym _))
		(vp-long (list 'sub (label-sym (path-sym s)) '*pc*))) *links*)
(vp-label 'func_syms)
	(each (lambda (s)
	(vp-label (str-sym _))
		(vp-string s) (vp-byte 0)) *strs*)
(vp-label 'func_paths)
	(each (lambda (s)
	(vp-label (path-sym _))
		(vp-string (str s)) (vp-byte 0)) *paths*)
	(vp-align ptr_size)
(vp-label 'func_end)
	(opt-emit-buffer)
	(when debug_emit
		(each print-emit *emit-buffer*))
	(emit-passes)
	(save *out-buffer* (cat "obj/" *os* "/" *cpu* "/" (str *func-name*))))

(defcfun fn-add-string (s)
	(defq i -1)
	(while (and (lt (setq i (inc i)) (length *strings*)) (not (eql s (elem i *strings*)))))
	(if (eq i (length *strings*)) (push *strings* s)) i)

(defcfun fn-add-sym (s)
	(defq i -1)
	(while (and (lt (setq i (inc i)) (length *strs*)) (not (eql s (elem i *strs*)))))
	(if (eq i (length *strs*)) (push *strs* s)) i)

(defcfun fn-add-path (p)
	(if (defq i (find p *paths*)) i
		(dec (length (push *paths* p)))))

(defcfun fn-add-link (p)
	(push *links* (fn-add-path p)))

(defcfun fn-add-atom (p)
	(push *atoms* (fn-add-sym p)))

(defcfun fn-find-link (p)
	(defq i -1)
	(while (and (lt (setq i (inc i)) (length *links*)) (not (eql p (elem (elem i *links*) *paths*)))))
	(if (eq i (length *links*)) (fn-add-link p)) i)

(defcfun fn-find-atom (p)
	(defq i -1)
	(while (and (lt (setq i (inc i)) (length *atoms*)) (not (eql p (elem (elem i *atoms*) *strs*)))))
	(if (eq i (length *atoms*)) (fn-add-atom p)) i)

(defcfun fn-string (s r)
	(vp-lea-p (string-sym (fn-add-string s)) r))

(defcfun fn-atom (a r)
	(vp-cpy-pr (atom-sym (fn-find-atom (str a))) r))

(defcfun fn-bind (p r)
	(if (def? p) (throw "Can't bind to inline function !" p)
		(vp-cpy-pr (link-sym (fn-find-link p)) r)))

(defcfun fn-call (p)
	(if (def? p) ((eval p))
		(vp-call-p (link-sym (fn-find-link p)))))

(defcfun fn-jmp (p)
	(if (def? p) (throw "Can't jump to inline function !" p)
		(vp-jmp-p (link-sym (fn-find-link p)))))

(defcfun abort (&optional s)
	(f-call 'pii 'write_str (list 2 (opt s "Abort !")))
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
