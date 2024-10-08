;;;;;;;;;;;;;;;;;;;;
; vp code translator
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun emit-label (label) (set (penv) label *pc*))

(defun emit-align (size &optional fill)
	(defq pc *pc* fill (opt fill 0) stream *out_stream*)
	(setq *pc* (align pc size))
	(while (<= (setq pc (inc pc)) *pc*)
		(write-char stream fill)))

(defun emit-string (string)
	(write *out_stream* string)
	(setq *pc* (+ *pc* (length string))))

(defmacro emit-data (type)
	(defq func_sym (sym (cat "emit-" type)) size_sym (sym (cat "+" type "_size")))
	(if (eql type 'byte)
		`(defun ,func_sym (&rest data)
			(write-char *out_stream* data)
			(setq *pc* (+ *pc* (length data))))
		`(defun ,func_sym (&rest data)
			(write-char *out_stream* data ,size_sym)
			(setq *pc* (+ *pc* (* (length data) ,size_sym))))))

(emit-data byte) (emit-data short) (emit-data int) (emit-data long)

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
	(if (starts-with "emit" k) (def e k v)) e) (tolist (env)) (env 1)))

(defun emit-translate (emit_code)
	; (emit-translate emit_code) -> func_binary
	(defq out_buf "" old_out_buf "" *pass* 0 *pc* 0
		*branches* (cap 64 (list)) *out_stream* (string-stream out_buf))
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
					(push inst (dec (length (push *branches* 0))))))
			;bind emit func
			(elem-set inst 0 (def? op +emit_funcs)))
		(list emit_code) 1)
	;multi pass of native emit functions
	(eval emit_code)
	(setq out_buf (str *out_stream*) *pass* (inc *pass*))
	(until (eql out_buf old_out_buf)
		(task-slice)
		(defq b old_out_buf old_out_buf out_buf *out_stream* (string-stream b) *pc* 0)
		(eval emit_code)
		(setq out_buf (str *out_stream*) *pass* (inc *pass*)))
	out_buf)

;module
(export-symbols '(emit-translate))
(env-pop)
