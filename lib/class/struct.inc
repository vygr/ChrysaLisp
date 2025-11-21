;;;;;;;;;;;;;;;;;;;;;;;
; structures and fields
;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun new-field (type alloc &optional alignment)
	(setq base (align base (ifn alignment alloc)))
	(push syms (sym (cat name "_" field)) (sym (cat name "_" field "_t")))
	(push values (num-intern base) type)
	(++ base alloc))

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
			(defq field (second line) alloc (eval (third line)))
			(new-field (str alloc) alloc +byte_size))
		(union
			(defq union_base base max_base base)
			(each! (lambda (line) (setq max_base (max max_base (_structure union_base line))))
				(list line) 1)
			(setq base max_base))
		(:t (each! (lambda (field)
				(case (first line)
					(byte (new-field "b" +byte_size))
					(ubyte (new-field "B" +byte_size))
					(short (new-field "s" +short_size))
					(ushort (new-field "S" +short_size))
					(int (new-field "i" +int_size))
					(uint (new-field "I" +int_size))
					(long (new-field "l" +long_size))
					(ulong (new-field "L" +long_size))
					(ptr (new-field "p" +ptr_size))
					(pptr (new-field "pp" +ptr_size))
					(pubyte (new-field "pB" +ptr_size))
					(pushort (new-field "pS" +ptr_size))
					(puint (new-field "pI" +ptr_size))
					(pulong (new-field "pL" +ptr_size))
					(pbyte (new-field "pb" +ptr_size))
					(pshort (new-field "ps" +ptr_size))
					(pint (new-field "pi" +ptr_size))
					(plong (new-field "pl" +ptr_size))
					(nodeid (new-field "no" +node_id_size +byte_size))
					(netid (new-field "ne" +net_id_size +byte_size))))
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
	; (field-type)
	; convert to get-field/set-field args
	(static-qq
		(defq size 0 type (eval (sym (cat field "_t"))) type
			(case type
				("b" +byte_size) ("s" +short_size) ("i" +int_size) ("l" +long_size)
				("B" (const (neg +byte_size))) ("S" (const (neg +short_size)))
				("I" (const (neg +int_size))) ("L" (const (neg +long_size)))
				(("p" "pb" "ps" "pi" "pl" "pB" "pS" "pI" "pL" "pp") 0)
				("no" (defq size +node_id_size) 0)
				("ne" (defq size +net_id_size) 0)
				(:t (defq size (str-to-num type)) 0))
			field (eval field) offset (ifn offset +str_data))))

(defmacro getf (obj field &optional offset)
	; (getf obj field [offset]) -> value
	(field-type) (static-qq (get-field ,obj ,(+ field offset) ,type ,size)))

(defmacro setf (obj field val &optional offset)
	; (setf obj field value [offset]) -> obj
	(field-type) (static-qq (set-field ,obj ,(+ field offset) ,type ,size ,val)))

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
