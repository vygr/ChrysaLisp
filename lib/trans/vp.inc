;;;;;;;;;;;;;;;;;;;;
; vp code translator
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

;pre-seed all standard VP opcodes in the module environment
(each (# (def (env) %0 :nil)) +vp_emit_ops)

(defun emit-align (size &optional fill)
	(defq pc *pc* fill (ifn fill 0) stream *stream*)
	(setq *pc* (align pc size))
	(while (<= (++ pc) *pc*) (write-char stream fill)))

(defun emit-nop ())
(defun emit-trash (&ignore))
(defun emit-prepass (emit_list) emit_list)

(defun emit-label (label) (set (penv) label *pc*))
(defun emit-string (string) (++ *pc* (write-blk *stream* string)))
(defun emit-byte (&rest data) (++ *pc* (write-char *stream* data)))
(defun emit-short (&rest data) (++ *pc* (write-char *stream* data +short_size)))
(defun emit-int (&rest data) (++ *pc* (write-char *stream* data +int_size)))
(defun emit-long (&rest data) (++ *pc* (write-char *stream* data +long_size)))

(defmacro emitm-byte (&rest data)
	(if (= (length data) 1)
		(static-qq (++ *pc* (write-char *stream* ~data)))
		(static-qq (++ *pc* (write-char *stream* (list ~data))))))

(defmacro emitm-short (&rest data)
	(if (= (length data) 1)
		(static-qq (++ *pc* (write-char *stream* ~data +short_size)))
		(static-qq (++ *pc* (write-char *stream* (list ~data) +short_size)))))

(defmacro emitm-int (&rest data)
	(if (= (length data) 1)
		(static-qq (++ *pc* (write-char *stream* ~data +int_size)))
		(static-qq (++ *pc* (write-char *stream* (list ~data) +int_size)))))

(defmacro emitm-long (&rest data)
	(if (= (length data) 1)
		(static-qq (++ *pc* (write-char *stream* ~data +long_size)))
		(static-qq (++ *pc* (write-char *stream* (list ~data) +long_size)))))

;;;;;;;;;;;;;
; call fusion
;;;;;;;;;;;;;

;on a CPU with a link register a call saves the link on the stack around
;itself. When one call follows another in a straight line, the restore after
;the first and the save before the second cancel, so the first call can leave
;the link on the stack, -h, the last can restore it, -t, and any between do
;neither, -m. The stack is one slot deeper between them, so :rsp relative
;access there is adjusted. This is legal as VP code may not look at the
;return address on the stack. A translator that wants this defines the
;emit-<cpu>-call[-r|-i|-p]-[h|m|t] functions and calls (trans-fuse-calls)
;from its (emit-prepass).

(defun trans-call-fuse-map (emit_list cpu)
	; (trans-call-fuse-map emit_list cpu) -> map
	;call -> (no restore version, no save version), barrier -> :t
	(defq calls '(emit-call emit-call-r emit-call-i emit-call-p)
		variant (lambda (v) (map (lambda (c) (sym (cat "emit-" cpu (slice c 4 -1) v))) calls))
		hs (variant "-h") ms (variant "-m") ts (variant "-t"))
	(reduce (lambda (m name)
			(pinsert m name (cond
				((defq i (find name calls))
					(list (elem-get hs i) (elem-get ts i)))
				((defq i (find name ts))
					(list (elem-get ms i) :nil))
				((find name '(emit-label emit-alloc emit-free emit-push emit-pop emit-call-abi
						emit-jmp emit-jmp-r emit-jmp-i emit-jmp-p emit-ret emit-sync emit-brk
						emit-beq-cr emit-bne-cr emit-bge-cr emit-ble-cr emit-blt-cr emit-bgt-cr
						emit-beq-rr emit-bne-rr emit-bge-rr emit-ble-rr emit-blt-rr emit-bgt-rr
						emit-beq-ff emit-bne-ff emit-bge-ff emit-blt-ff emit-ble-ff emit-bgt-ff))
					:t))))
		emit_list (pmap)))

(defun trans-sp-map (emit_list)
	; (trans-sp-map emit_list) -> map
	;:rsp relative access -> (base index, offset index), or :cpy
	(reduce (lambda (m name)
			(pinsert m name (cond
				((find name '(emit-cpy-ir emit-cpy-ir-b emit-cpy-ir-s emit-cpy-ir-i
						emit-cpy-ir-ub emit-cpy-ir-us emit-cpy-ir-ui emit-lea-i emit-cpy-if))
					'(1 2))
				((find name '(emit-cpy-ri emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i emit-cpy-fi))
					'(2 3))
				((eql name 'emit-cpy-rr) :cpy))))
		emit_list (pmap)))

(defun trans-sp-adjust (inst sp_map)
	; (trans-sp-adjust inst sp_map) -> :fail | inst
	;the instruction, that uses :rsp, as it must be with the stack one slot
	;deeper, :fail if it can't be adjusted
	(cond
		((/= 1 (length (filter (# (eql %0 :rsp)) inst))) :fail)
		((not (defq info (pfind sp_map (first inst)))) :fail)
		((eql info :cpy)
			(if (eql (second inst) :rsp)
				(list 'emit-lea-i :rsp +ptr_size (third inst))
				:fail))
		((and (eql (elem-get inst (first info)) :rsp)
				(num? (defq c (elem-get inst (second info)))))
			(elem-set (setq inst (slice inst 0 -1)) (second info) (+ c +ptr_size))
			inst)
		(:t :fail)))

(defun trans-fuse-calls (emit_list call_map sp_map)
	; (trans-fuse-calls emit_list call_map sp_map) -> emit_list
	;one forward sweep, pend is the last call we could fuse the next call
	;with, adj the :rsp adjusted instructions since it
	(defq pend :nil pend_info :nil adj (list))
	(each! (lambda (inst)
		(defq k (!))
		(cond
			((not (defq info (pfind call_map (first inst))))
				;straight line code
				(and pend (find :rsp inst)
					(if (eql (defq a (trans-sp-adjust inst sp_map)) :fail)
						(setq pend :nil)
						(push adj k a))))
			((or (eql info :t) (find :rsp inst))
				;barrier
				(setq pend :nil))
			(pend
				;second call of a pair, and maybe the first of the next
				(elem-set emit_list pend (cat (list (first pend_info)) (rest (elem-get emit_list pend))))
				(elem-set emit_list k (cat (list (second info)) (rest inst)))
				(each (lambda ((i a)) (elem-set emit_list i a)) (partition adj 2))
				(clear adj)
				(setq pend k pend_info (pfind call_map (second info))))
			(:t ;first call
				(clear adj)
				(setq pend k pend_info info))))
		(list emit_list) 9)
	emit_list)

;;;;;;;;;;;;;;;;;;;;;;;
; native emit functions
;;;;;;;;;;;;;;;;;;;;;;;

(if (/= 0 (age (defq emit_file (cat "lib/trans/" *cpu* ".inc"))))
	(import emit_file)
	(throw "No such CPU !" *cpu*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; gather all the emit funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;map with all emit-* instructions
(defq +emit_funcs (reduce (lambda (%0 (%1 %2))
		(if (starts-with "emit-" %1) (pinsert %0 %1 %2) %0))
	(tolist (env)) (pmap)))

;emap with jump/branch instructions flagged
(defq +emit_jmps (reduce (lambda (%0 (%1 &)) (pinsert %0 %1 (find %1
		'(emit-jmp emit-beq-cr emit-bne-cr emit-bne-rr emit-beq-rr
		emit-bge-rr emit-ble-rr emit-bge-cr emit-ble-cr emit-blt-cr
		emit-blt-rr emit-bgt-rr emit-bgt-cr emit-beq-ff emit-bne-ff
		emit-bge-ff emit-blt-ff emit-ble-ff emit-bgt-ff))))
	(partition +emit_funcs 2) (pmap)))

;validate that all instruction maps have identical symbols and slots
(validatecase
	(defq emit_lists (map (# (map (const first) %0))
		(map (# (partition %0 2)) (cat (list +emit_funcs +emit_jmps) emit_maps))))
	(unless (and (apply (const =) (map (const length) emit_lists))
				(every (# (apply (const =) %0))
					(map! (lambda (&rest %0) (map (const weak-ref) %0)) emit_lists)))
		(throw "Mismatching instruction maps !" emit_lists)))

(defun emit-translate (emit_code)
	; (emit-translate emit_code) -> func_binary
	(defq emit_code (emit-prepass emit_code)
		buf "" last_buf "" *offsets* (cap 64 (list))
		*pass* 0 *stream* (string-stream buf) *pc* 0)
	(each! (lambda (inst)
			;map vp reg syms to native reg nums
			(each! (# (if (defq %0 (emit-native-reg? %0)) (elem-set inst (!) %0)))
				(list inst) 1)
			;create label and branch initial values
			(cond
				((eql (defq op (first inst)) 'emit-label)
					(def (penv) (last (last inst)) 0))
				((pfind +emit_jmps op)
					(push inst (dec (length (push *offsets* 0))))))
			;bind emit func
			(elem-set inst 0 (pfind +emit_funcs op)))
		(list emit_code) 1)
	;multi pass of native emit functions
	(eval emit_code)
	(setq buf (str *stream*) *pass* (inc *pass*))
	(until (eql buf last_buf)
		(task-slice)
		(defq b last_buf last_buf buf *stream* (string-stream b) *pc* 0)
		(eval emit_code)
		(setq buf (str *stream*) *pass* (inc *pass*)))
	buf)

;module
(export-symbols '(emit-translate))
(env-pop)
