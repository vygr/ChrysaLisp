;;;;;;;;;;;;;;;;;;;;
; vp code translator
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun emit-label (_)
	(set (penv) _ *pc*))

(defun emit-byte (&rest _)
	(write-char *out_stream* _)
	(setq *pc* (+ *pc* (length _))))

(defun emit-short (&rest _)
	(write-char *out_stream* _ 2)
	(setq *pc* (+ *pc* (* (length _) 2))))

(defun emit-int (&rest _)
	(write-char *out_stream* _ 4)
	(setq *pc* (+ *pc* (* (length _) 4))))

(defun emit-long (&rest _)
	(write-char *out_stream* _ 8)
	(setq *pc* (+ *pc* (* (length _) 8))))

(defun emit-string (_)
	(write *out_stream* _)
	(setq *pc* (+ *pc* (length _))))

(defun emit-align (a &optional b)
	(defq p *pc* b (opt b 0) s *out_stream*)
	(setq *pc* (align p a))
	(while (<= (setq p (inc p)) *pc*)
		(write-char s b)))

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
		*branches* (cap 64 (list))
		*out_stream* (string-stream out_buf))
	(each! (lambda (op)
			;map vp reg syms to native reg nums
			(each! (lambda (e)
				(if (defq e (emit-native-reg? e))
					(elem-set op (!) e))) (list op) 1)
			;create label and branch initial values
			(cond
				((eql (defq e (first op)) 'emit-label)
					(def (penv) (last (last op)) 0))
				((find e '(emit-beq-cr emit-bne-cr emit-bge-cr emit-blt-cr emit-ble-cr emit-bgt-cr
						emit-beq-rr emit-bne-rr emit-bge-rr emit-blt-rr emit-ble-rr emit-bgt-rr emit-jmp))
					(push op (dec (length (push *branches* 0))))))
			;bind emit func
			(elem-set op 0 (def? e +emit_funcs)))
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
