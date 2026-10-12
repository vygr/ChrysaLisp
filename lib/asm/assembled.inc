;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a function as it was assembled, read back
;
; A build of the VP64 leaves each function's instructions as text in
; obj/vp/, 'make vp'. This reads one, and says where control goes from
; each instruction. It is what the commands that see a function whole
; stand on, (locals) and (relies).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +obj_dir "obj/vp/" +jit_dir "lib/gpu/jit/"
	;what reads a place of the frame, what writes one, and what takes
	;the address of one: each has the stack pointer as its base
	+load_ops ''(emit-cpy-ir emit-cpy-ir-b emit-cpy-ir-s emit-cpy-ir-i
		emit-cpy-ir-ub emit-cpy-ir-us emit-cpy-ir-ui emit-cpy-if)
	+store_ops ''(emit-cpy-ri emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i emit-cpy-fi)
	;what goes somewhere and does not come back here, and what calls
	+leave_ops ''(emit-ret emit-jmp-p emit-jmp-r emit-jmp-i)
	+call_ops ''(emit-call emit-call-p emit-call-r emit-call-i emit-call-abi)
	;what is not an instruction of the function at all
	+data_ops ''(emit-label emit-tlabel emit-align emit-string emit-byte emit-short emit-int emit-long))

(defun insts-of (function)
	; (insts-of function) -> :nil | (insts labels start frames)
	;the instructions of a function, from where it is entered to where
	;its table of links begins. The fourth thing of the file is the
	;header, which has the two labels. And what its frames of named
	;locals were, the form after the instructions, if it has any: each
	;(which_alloc size ((name offset type) ...))
	(when (and (defq stream (file-stream (cat +obj_dir function)))
			(defq all (first (read stream))) (> (length all) 4))
		(defq head (elem-get all 3) labels (Fmap 31) more (first (read stream)))
		(each (# (if (find (first %0) '(emit-label emit-tlabel)) (. labels :insert (last (second %0)) (!)))) all)
		(and (eql (first head) 'emit-short) (>= (length head) 4)
			(defq start (. labels :find (third head))) (defq end (. labels :find (elem-get head 3)))
			(list (slice all start end) labels start
				(if (and (list? more) (nempty? more) (eql (first more) 'locals)) (rest more) (list)) all))))

(defun regs-of (text)
	; (regs-of text) -> (num ...)
	;the general registers in what a method is declared to trash, as
	;(format-regs) writes them, ":r1-:r8, :r14, :f0-:f15": their numbers
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
			((eql op 'emit-jmp) (if (defq to (target-of inst labels start count)) (list to) (list)))
			((starts-with "emit-b" op)
				(if (defq to (target-of inst labels start count)) (push on to) on))
			(on)))
		insts))

;module
(export-symbols '(insts-of regs-of target-of next-of
	+obj_dir +jit_dir +load_ops +store_ops +leave_ops +call_ops +data_ops))
(env-pop)
