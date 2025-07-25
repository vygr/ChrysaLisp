;;;;;;;;;;;;;;;;;;;;;;;
; structures and fields
;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun new_field (type size alignment)
	(setq base (align base alignment))
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
					(byte (new_field "b" +byte_size +byte_size))
					(ubyte (new_field "B" +byte_size +byte_size))
					(short (new_field "s" +short_size +short_size))
					(ushort (new_field "S" +short_size +short_size))
					(int (new_field "i" +int_size +int_size))
					(uint (new_field "I" +int_size +int_size))
					(long (new_field "l" +long_size +long_size))
					(ulong (new_field "L" +long_size +long_size))
					(ptr (new_field "p" +ptr_size +ptr_size))
					(pptr (new_field "pp" +ptr_size +ptr_size))
					(pubyte (new_field "pB" +ptr_size +ptr_size))
					(pushort (new_field "pS" +ptr_size +ptr_size))
					(puint (new_field "pI" +ptr_size +ptr_size))
					(pulong (new_field "pL" +ptr_size +ptr_size))
					(pbyte (new_field "pb" +ptr_size +ptr_size))
					(pshort (new_field "ps" +ptr_size +ptr_size))
					(pint (new_field "pi" +ptr_size +ptr_size))
					(plong (new_field "pl" +ptr_size +ptr_size))
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
	`(bind ',syms ',values))

(defmacro getf (s field &optional o)
	; (getf obj field [offset]) -> value
	(defq type (eval (sym (cat field "_t"))) field (eval field) o (ifn o +str_data))
	(case (sym type)
		(b `(get-field ,s ,(+ o field) ,+byte_size))
		(s `(get-field ,s ,(+ o field) ,+short_size))
		(i `(get-field ,s ,(+ o field) ,+int_size))
		(l `(get-field ,s ,(+ o field) ,+long_size))
		(B `(get-field ,s ,(+ o field) ,(neg +byte_size)))
		(S `(get-field ,s ,(+ o field) ,(neg +short_size)))
		(I `(get-field ,s ,(+ o field) ,(neg +int_size)))
		(L `(get-field ,s ,(+ o field) ,(neg +long_size)))
		((p pp pb pB ps pS pi pI pl pL) `(get-field ,s ,(+ o field) 0))
		((no) `(get-field ,s ,(+ o field) ,+node_id_size))
		((ne) `(get-field ,s ,(+ o field) ,+net_id_size))))

(defmacro setf (s field v &optional o)
	; (setf obj field value [offset]) -> obj
	(defq type (eval (sym (cat field "_t"))) field (eval field) o (ifn o +str_data))
	(case (sym type)
		(b `(set-field ,s ,(+ o field) ,+byte_size ,v))
		(s `(set-field ,s ,(+ o field) ,+short_size ,v))
		(i `(set-field ,s ,(+ o field) ,+int_size ,v))
		(l `(set-field ,s ,(+ o field) ,+long_size ,v))
		(B `(set-field ,s ,(+ o field) ,(neg +byte_size) ,v))
		(S `(set-field ,s ,(+ o field) ,(neg +short_size) ,v))
		(I `(set-field ,s ,(+ o field) ,(neg +int_size) ,v))
		(L `(set-field ,s ,(+ o field) ,(neg +long_size) ,v))
		((p pp pb pB ps pS pi pI pl pL) `(set-field ,s ,(+ o field) 0 ,v))
		((no) `(set-field ,s ,(+ o field) ,+node_id_size ,v))
		((ne) `(set-field ,s ,(+ o field) ,+net_id_size ,v))))

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
	`(bind ',syms ',values))

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
	`(bind ',syms ',values))

(defmacro bits? (val &rest masks)
	; (bits? val &rest masks) -> :t | :nil
	(if (> (length masks) 1)
		`(/= 0 (logand ,val (const (num-intern (logior ~masks)))))
		`(/= 0 (logand ,val ~masks))))

;module
(export-symbols '(structure _structure getf setf setf-> enums bits bits?))
(env-pop)
