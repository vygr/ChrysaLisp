;;;;;;;;;;;;;;;;;;;;
; vp code translator
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun emit-align (size &optional fill)
	(defq pc *pc* fill (ifn fill 0) stream *stream*)
	(setq *pc* (align pc size))
	(while (<= (++ pc) *pc*) (write-char stream fill)))

(defun emit-label (label) (set (penv) label *pc*))
(defun emit-string (string) (setq *pc* (+ *pc* (write *stream* string))))
(defun emit-byte (&rest data) (setq *pc* (+ *pc* (write-char *stream* data))))
(defun emit-short (&rest data) (setq *pc* (+ *pc* (write-char *stream* data +short_size))))
(defun emit-int (&rest data) (setq *pc* (+ *pc* (write-char *stream* data +int_size))))
(defun emit-long (&rest data) (setq *pc* (+ *pc* (write-char *stream* data +long_size))))

(defmacro emitm-byte (&rest data)
	(if (= (length data) 1)
		(static-qq (setq *pc* (+ *pc* (write-char *stream* ~data))))
		(static-qq (setq *pc* (+ *pc* (write-char *stream* (list ~data)))))))

(defmacro emitm-short (&rest data)
	(if (= (length data) 1)
		(static-qq (setq *pc* (+ *pc* (write-char *stream* ~data +short_size))))
		(static-qq (setq *pc* (+ *pc* (write-char *stream* (list ~data) +short_size))))))

(defmacro emitm-int (&rest data)
	(if (= (length data) 1)
		(static-qq (setq *pc* (+ *pc* (write-char *stream* ~data +int_size))))
		(static-qq (setq *pc* (+ *pc* (write-char *stream* (list ~data) +int_size))))))

(defmacro emitm-long (&rest data)
	(if (= (length data) 1)
		(static-qq (setq *pc* (+ *pc* (write-char *stream* ~data +long_size))))
		(static-qq (setq *pc* (+ *pc* (write-char *stream* (list ~data) +long_size))))))

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
				((find op '(emit-jmp emit-beq-cr emit-bne-cr emit-bne-rr emit-beq-rr
						emit-bge-rr emit-ble-rr emit-bge-cr emit-ble-cr
						emit-blt-cr emit-blt-rr emit-bgt-rr emit-bgt-cr))
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
