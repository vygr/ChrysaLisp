;;;;;;;;;;;;;;;;;;;;;;;
; structures and fields
;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun new_field (type size &optional alignment)
	(setq base (align base (ifn alignment size)))
	(push syms (sym (cat name "_" field)) (sym (cat name "_" field "_t")))
	(push values (num-intern base) type)
	(++ base size))

(defun _structure (base line)
	(case (first line)
		(align
			(if (> (length line) 1)
				(setq base (align base (eval (second line))))
				(setq base (align base +long_size))))
		(offset
			(push syms (sym (cat name "_" (second line))))
			(push values (num-intern base)))
		(struct
			(push syms (sym (cat name "_" (second line))) (sym (cat name "_" (second line) "_t")))
			(push values (num-intern base) :nil)
			(setq base (+ base (eval (third line)))))
		(union
			(defq union_base base max_base base)
			(each! (lambda (line) (setq max_base (max max_base (_structure union_base line))))
				(list line) 1)
			(setq base max_base))
		(:t (each! (lambda (field)
				(case (first line)
					(byte (new_field "b" +byte_size))
					(ubyte (new_field "B" +byte_size))
					(short (new_field "s" +short_size))
					(ushort (new_field "S" +short_size))
					(int (new_field "i" +int_size))
					(uint (new_field "I" +int_size))
					(long (new_field "l" +long_size))
					(ulong (new_field "L" +long_size))
					(ptr (new_field "p" +ptr_size))
					(pptr (new_field "pp" +ptr_size))
					(pubyte (new_field "pB" +ptr_size))
					(pushort (new_field "pS" +ptr_size))
					(puint (new_field "pI" +ptr_size))
					(pulong (new_field "pL" +ptr_size))
					(pbyte (new_field "pb" +ptr_size))
					(pshort (new_field "ps" +ptr_size))
					(pint (new_field "pi" +ptr_size))
					(plong (new_field "pl" +ptr_size))
					(nodeid (new_field "no" +node_id_size +byte_size))
					(netid (new_field "ne" +net_id_size +byte_size))))
				(list line) 1))) base)

(defmacro structure (name base &rest lines)
	; (structure name base [(byte field ...)] ...)
	(if (def? (sym (cat name "_size")) (penv))
		(throw "Structure redefined !" name))
	(defq syms (list) values (list) base (eval base))
	(each (lambda (line)
		(setq base (_structure base line))) lines)
	(push syms (sym (cat name "_size")))
	(push values (num-intern base))
	(static-qq (bind ',syms ',values)))

(defmacro field-type ()
	(static-qq (defq type (eval (sym (cat f "_t"))) f (eval f) o (ifn o +str_data)
		type (case (sym type)
			(b +byte_size) (s +short_size) (i +int_size) (l +long_size)
			(B (const (neg +byte_size))) (S (const (neg +short_size)))
			(I (const (neg +int_size))) (L (const (neg +long_size)))
			((no) +node_id_size) ((ne) +net_id_size) (:t 0)))))

(defmacro getf (s f &optional o)
	; (getf obj field [offset]) -> value
	(field-type) (static-qq (get-field ,s ,(+ o f) ,type)))

(defmacro setf (s f v &optional o)
	; (setf obj field value [offset]) -> obj
	(field-type) (static-qq (set-field ,s ,(+ o f) ,type ,v)))

(defmacro setf-> (res &rest frm)
	; (setf-> msg form ...)
	(reduce (lambda (res frm)
		(if (list? frm)
			(insert frm 0 (list 'setf res))
			(list 'setf res frm))) frm res))

;;;;;;;
; enums
;;;;;;;

(defun new_enum (size)
	(push syms (sym (cat name "_" field)))
	(push values (num-intern base))
	(++ base size))

(defmacro enums (name base &rest lines)
	; (enums name base [(enum field ...)] ...)
	(if (def? (sym (cat name "_size")) (penv))
		(throw "Enums redefined !" name))
	(defq syms (list) values (list) base (eval base))
	(each (lambda (line)
		(each! (lambda (field)
			(case (first line)
				(enum (new_enum +byte_size))))
			(list line) 1)) lines)
	(push syms (sym (cat name "_size")))
	(push values (num-intern base))
	(static-qq (bind ',syms ',values)))

;;;;;;
; bits
;;;;;;

(defun new_bit ()
	(push syms (sym (cat name "_" field)))
	(push values (num-intern (<< 1 base)))
	(++ base))

(defmacro bits (name base &rest lines)
	; (bits name base [(bit field ...)] ...)
	(if (def? (sym (cat name "_size")) (penv))
		(throw "Bits redefined !" name))
	(defq syms (list) values (list) base (eval base))
	(each (lambda (line)
		(each! (lambda (field)
			(case (first line)
				(bit (new_bit))))
			(list line) 1)) lines)
	(push syms (sym (cat name "_size")))
	(push values (num-intern base))
	(static-qq (bind ',syms ',values)))

(defun bit-mask (&rest masks)
	; (bit-mask mask ...) -> val
	(num-intern (apply (const logior) masks)))

(defmacro bits? (val &rest masks)
	; (bits? val mask ...) -> :t | :nil
	(if (> (length masks) 1)
		(static-qq (/= 0 (logand ,val ,(num-intern (apply (const logior) (eval-list masks))))))
		(static-qq (/= 0 (logand ,val ~masks)))))

;module
(export-symbols '(structure _structure getf setf setf-> enums bits bits? bit-mask))
(env-pop)
