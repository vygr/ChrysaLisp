;;;;;;;;;;;;;;;;;;;;
; vp code translator
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun emit-label (label) (set (penv) label *pc*))

(defun emit-align (size &optional fill)
	(defq pc *pc* fill (opt fill 0) stream *stream*)
	(setq *pc* (align pc size))
	(while (<= (setq pc (inc pc)) *pc*)
		(write-char stream fill)))

(defun emit-string (string)
	(setq *pc* (+ *pc* (write *stream* string))))

(defmacro data-func (type)
	(defq func_sym (sym (cat "emit-" type))
		size (eval (sym (cat "+" type "_size"))))
	(if (eql type 'byte)
		`(defun ,func_sym (&rest data)
			(setq *pc* (+ *pc* (write-char *stream* data))))
		`(defun ,func_sym (&rest data)
			(setq *pc* (+ *pc* (write-char *stream* data ,size))))))

(defmacro data-macro (type)
	(defq func_sym (sym (cat "emitm-" type))
		size (eval (sym (cat "+" type "_size"))))
	(if (eql type 'byte)
		`(defmacro ,func_sym (&rest data)
			(if (= (length data) 1)
				`(setq *pc* (+ *pc* (write-char *stream* ~data)))
				`(setq *pc* (+ *pc* (write-char *stream* (list ~data))))))
		`(defmacro ,func_sym (&rest data)
			(if (= (length data) 1)
				`(setq *pc* (+ *pc* (write-char *stream* ~data ,,size)))
				`(setq *pc* (+ *pc* (write-char *stream* (list ~data) ,,size)))))))

(data-func byte) (data-func short) (data-func int) (data-func long)
(data-macro byte) (data-macro short) (data-macro int) (data-macro long)

;;;;;;;;;;;;;;;;;;;;;;;
; native emit functions
;;;;;;;;;;;;;;;;;;;;;;;

(if (/= 0 (age (defq emit_file (cat "lib/trans/" *cpu* ".inc"))))
	(import emit_file)
	(throw "No such CPU !" *cpu*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; gather all the emit funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +emit_funcs (reduce (lambda (e (k v))
	(if (starts-with "emit-" k) (def e k v)) e) (tolist (env)) (env 1)))

(defun emit-translate (emit_code)
	; (emit-translate emit_code) -> func_binary
	(defq buf "" last_buf "" *offsets* (cap 64 (list))
		*pass* 0 *stream* (string-stream buf) *pc* 0)
	(each! (lambda (inst)
			;map vp reg syms to native reg nums
			(each! (# (if (defq %0 (emit-native-reg? %0)) (elem-set inst (!) %0)))
				(list inst) 1)
			;create label and branch initial values
			(cond
				((eql (defq op (first inst)) 'emit-label)
					(def (penv) (last (last inst)) 0))
				((find op '(emit-beq-cr emit-bne-cr emit-bge-cr emit-blt-cr
						emit-ble-cr emit-bgt-cr emit-beq-rr emit-bne-rr
						emit-bge-rr emit-blt-rr emit-ble-rr emit-bgt-rr emit-jmp))
					(push inst (dec (length (push *offsets* 0))))))
			;bind emit func
			(elem-set inst 0 (def? op +emit_funcs)))
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
