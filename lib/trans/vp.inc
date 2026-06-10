;;;;;;;;;;;;;;;;;;;;
; vp code translator
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun emit-align (size &optional fill)
	(defq pc *pc* fill (ifn fill 0) stream *stream*)
	(setq *pc* (align pc size))
	(while (<= (++ pc) *pc*) (write-char stream fill)))

(defun emit-nop ())
(defun emit-trash (&ignore))
(defun emit-prepass (emit_list) emit_list)
(defun emit-label (label) (set (penv) label *pc*))
(defun emit-tlabel (temp) (set (penv) temp *pc*))
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

;;;;;;;;;;;;;;;;;;;;;;;
; native emit functions
;;;;;;;;;;;;;;;;;;;;;;;

(if (/= 0 (age (defq emit_file (cat "lib/trans/" *cpu* ".inc"))))
	(import emit_file)
	(throw "No such CPU !" *cpu*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; gather all the emit funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;env with all emit-* instructions
(defq +emit_funcs (reduce (lambda (e (k v)) (if (starts-with "emit-" k) (pinsert e k v) e))
	(tolist (env)) (plist)))

;env with jump/branch instructions flagged
(defq +emit_jmps (reduce (lambda (e (k v)) (pinsert e k (find k
		'(emit-jmp emit-beq-cr emit-bne-cr emit-bne-rr emit-beq-rr
		emit-bge-rr emit-ble-rr emit-bge-cr emit-ble-cr emit-blt-cr
		emit-blt-rr emit-bgt-rr emit-bgt-cr emit-min-cr emit-max-cr
		emit-min-rr emit-max-rr emit-abs-rr emit-beq-ff emit-bne-ff
		emit-bge-ff emit-blt-ff emit-ble-ff emit-bgt-ff))))
	(partition +emit_funcs 2) (plist)))

;validate that all instruction maps have identical symbols and slots
(validatecase
	(defq emit_lists (map (# (map (const first) %0)) (map (const tolist)
		(cat (list +emit_funcs +emit_jmps) emit_maps))))
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
				((find (defq op (first inst)) '(emit-label emit-tlabel))
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
