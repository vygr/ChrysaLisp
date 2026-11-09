;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a function as it was assembled, read back
;
; A build keeps the VP of each source file's functions as text, a bundle
; of them under the file's name in obj/vp/, a validate one's beside it in
; obj/vp_validate/, see lib/asm/asm.inc. This finds a function's, reads it, and says where control goes from
; each instruction, and what a call in it is of and is declared to trash,
; take and give. It is what the commands that see a function whole stand
; on, (locals) and (relies).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +obj_dir "obj/vp/" +jit_dir "lib/gpu/jit/"
	;the general registers, a register as a bit is 1 shifted by its place
	+regs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14)
	;what reads a place of the frame, what writes one, and what takes
	;the address of one: each has the stack pointer as its base
	+load_ops ''(load-rcr load-rcr-b load-rcr-s load-rcr-i
		load-rcr-ub load-rcr-us load-rcr-ui load-rcd load-rcf)
	+store_ops ''(store-rrc store-rrc-b store-rrc-s store-rrc-i store-drc store-frc)
	;what goes somewhere and does not come back here, and what calls
	+leave_ops ''(ret jmp-p jmp-r jmp-rc)
	+call_ops ''(call call-p call-r call-rc call-abi)
	;what is not an instruction of the function at all
	+data_ops ''(label tlabel align string byte short int long))

(defun vp-root (functions)
	; (vp-root functions) -> path
	;the folder of VP the functions named are of, obj/vp_validate/ if they
	;are bundles of a validate build, else a normal build's, obj/vp/. What
	;(vp-index) reads is *vp_root*, a normal build's unless this is bound
	(if (some (# (starts-with "obj/vp_validate/" %0)) functions) "obj/vp_validate/" +obj_dir))

(defun vp-index ()
	; (vp-index) -> fmap
	;of each function, the bundle its VP is in, of *vp_root*, found once.
	;An entry of a bundle starts with a line of the name, ("name"
	(memoize :index (progn
			(defq index (Fmap 101))
			(each (lambda (path)
					(lines! (# (if (starts-with "(\q" %0)
								(. index :insert (slice %0 2 -2) path)) :nil) (file-stream path)))
				(files-all (slice *vp_root* 0 -2) '("")))
			index) 1))

(defun vp-listing (function)
	; (vp-listing function) -> :nil | (insts form ...)
	;the VP of a function, its instructions and the forms after them
	(when (defq path (. (vp-index) :find function))
		(defq stream (file-stream path) found :nil)
		(while (and (not found) (defq entry (first (read stream))))
			(if (eql (first entry) function) (setq found (rest entry))))
		found))

(defun vp-functions-of (args)
	; (vp-functions-of args) -> (function ...)
	;the functions named, a bundle's own, obj/vp/<file>, for each of
	;a bundle given, or all of them for none
	(cond
		((empty? args)
			(defq all (list))
			(. (vp-index) :each (lambda (k v) (push all k)))
			(sort all))
		((reduce (lambda (out arg)
					(cond
						((and (starts-with *vp_root* arg) (/= (age arg) 0))
							(lines! (# (if (starts-with "(\q" %0)
										(push out (slice %0 2 -2))) :nil) (file-stream arg))
							out)
						((push out arg)))) args (list)))))

(defun insts-of (function)
	; (insts-of function) -> :nil | (insts labels start frames)
	;the instructions of a function, from where it is entered to where
	;its table of links begins. The fourth thing of the file is the
	;header, which has the two labels. And what its frames of named
	;locals were, the form after the instructions, if it has any: each
	;(which_alloc size ((name offset type) ...))
	(when (and (defq listing (vp-listing function))
			(defq all (first listing)) (> (length all) 3))
		(defq head (elem-get all 2) labels (Fmap 31) more (second listing))
		(each (# (if (find (first %0) '(label tlabel)) (. labels :insert (last (second %0)) (!)))) all)
		(and (eql (first head) 'short) (>= (length head) 4)
			(defq start (. labels :find (third head))) (defq end (. labels :find (elem-get head 3)))
			(list (slice all start end) labels start
				(if (and (list? more) (nempty? more) (eql (first more) 'locals)) (rest more) (list)) all))))

(defun regs-of (text)
	; (regs-of text) -> (num ...)
	;the general registers in what a method is declared to trash, as
	;(format-regs) writes them, ":r1-:r8, :r14, :d0-:d15": their numbers
	(reduce (lambda (out part)
			(when (starts-with ":r" part)
				(defq ends (split part "-") from (str-as-num (slice (first ends) 2 -1)) to (str-as-num (slice (last ends) 2 -1)))
				(each (# (push out %0)) (range from (inc to))))
			out) (split text (const (char-class ", "))) (list)))

(defun target-of (inst labels start count)
	; (target-of inst labels start count) -> :nil | index
	;where a branch or a jump goes, if that is in the function
	(and (sym? (defq label (last inst))) (defq at (. labels :find label))
		(<= 0 (setq at (- at start))) (< at count) at))

(defun next-of (insts labels start)
	; (next-of insts labels start) -> ((index ...) ...)
	;for each instruction, where control may go from it
	(defq count (length insts))
	(map (lambda (inst)
			(defq op (first inst) at (!) on (if (< (inc at) count) (list (inc at)) (list)))
			(cond
				((find op +leave_ops) (list))
				((eql op 'jmp) (if (defq to (target-of inst labels start count)) (list to) (list)))
				((and (starts-with "b" op) (nql op 'byte))
					(if (defq to (target-of inst labels start count)) (push on to) on))
				(on)))
		insts))

(defun bit-of (reg)
	; (bit-of reg) -> num
	;a general register as a bit, 0 if it is not one
	(if (and (sym? reg) (defq i (find reg +regs))) (<< 1 i) 0))

(defun bits-of (regs)
	; (bits-of regs) -> num
	;registers, or their numbers, as bits
	(reduce (# (logior %0 (if (num? %1) (<< 1 %1) (bit-of %1)))) regs 0))

(defun declared-of (class_db class method key)
	; (declared-of class_db class method key) -> num
	;what a method takes, :dec_inputs, or gives, :dec_outputs, as bits:
	;as its class declares it, or the nearest class above that says
	;
	;If none of them says, what the comment of the function that is the
	;method says: a create is declared with neither, and its comment has
	;what it gives
	(defq said "none" from class static :nil)
	;a static method of a class is not the one of that name in the class
	;above, a create: only a virtual one takes and gives what that does
	(while (and (eql said "none") (not static) class (nql class :nil) (defq entry (. class_db :find class)))
		(when (defq m (.-> entry (:find :methods) (:find method)))
			(setq said (ifn (. m :find key) "none") static (eql (. m :find :type) :static)))
		(setq class (. entry :find :parent)))
	(and (eql said "none") (defq entry (. class_db :find from))
		(defq m (.-> entry (:find :methods) (:find method)))
		(setq said (ifn (. m :find (if (eql key :dec_inputs) :inputs :outputs)) "none")))
	;a create that says nothing gives the object, as they all do
	(and (eql said "none") (eql key :dec_outputs) (starts-with ":create" (str method)) (setq said ":r0"))
	(bits-of (regs-of said)))

(defun methods-of (class_db)
	; (methods-of class_db) -> (by_function class_db)
	;what is declared of each method, by the function it is: (trashes
	;takes gives), as bits. trashes is what the function itself does if
	;that is said, a virtual method's, and :nil if nothing is said
	(defq by_function (Fmap 1021))
	(. class_db :each (lambda (class entry)
			(.-> entry (:find :methods) (:each (lambda (method m)
						(when (. m :find :dec_line)
							(defq said (ifn (. m :find :declared_own) (. m :find :declared)) path (str (. m :find :function))
								now (list (if said (bits-of (regs-of said)))
									(declared-of class_db class method :dec_inputs)
									(declared-of class_db class method :dec_outputs))
								was (. by_function :find path))
							;a function may be more than one method, a create that is a
							;vcreate too, and a call bound to it does not say which. What
							;it trashes is what any of them says, what it gives what any
							;does, and what it takes only what they all do: so less is
							;said to be gone by here than may be, never more
							(. by_function :insert path (if was
									(list (if (or (first was) (first now)) (logior (ifn (first was) 0) (ifn (first now) 0)))
										(logand (second was) (second now)) (logior (third was) (third now)))
									now))))))))
	(list by_function class_db))

(defun called-of (inst all labels methods)
	; (called-of inst all labels methods) -> :nil | (name trashes takes gives)
	;what a call or a jump out is of, and what is declared of it: one
	;bound to its function, through a link to its path, and what that
	;function trashes; or one through a vtable, which has its class and
	;name with it, and what any class called so may. :nil if not known
	(bind '(by_function class_db &ignore) methods)
	(case (first inst)
		((call-p jmp-p)
			(and (defq at (. labels :find (second inst))) (< (inc at) (length all))
				(list? (defq to (second (elem-get all (inc at))))) (> (length to) 1)
				(defq at (. labels :find (second to))) (< (inc at) (length all))
				(str? (defq path (second (elem-get all (inc at)))))
				(defq m (. by_function :find path)) (first m)
				(cat (list path) m)))
		((call-rc jmp-rc)
			(when (> (length inst) 4)
				(defq class (elem-get inst 3) method (elem-get inst 4) found :nil of class
					name (cat (str class) " " (str method) ", by its vtable"))
				;said by the class, or the nearest above it that does
				(while (and (not found) class (nql class :nil) (defq entry (. class_db :find class)))
					(and (defq m (.-> entry (:find :methods) (:find method))) (. m :find :declared) (setq found m))
					(setq class (. entry :find :parent)))
				(if found (list name (bits-of (regs-of (. found :find :declared)))
						(declared-of class_db of method :dec_inputs)
						(declared-of class_db of method :dec_outputs)))))))

;module
(defq *vp_root* +obj_dir)
(export-symbols '(*vp_root* insts-of vp-root vp-index vp-listing vp-functions-of regs-of target-of next-of
		bit-of bits-of declared-of methods-of called-of +regs
		+obj_dir +jit_dir +load_ops +store_ops +leave_ops +call_ops +data_ops))
(env-pop)
