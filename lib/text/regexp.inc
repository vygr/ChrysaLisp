;;;;;;;;;;;;;;;;;;;;;
; regexp search class
;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +op_presidence ".+*?|()" +op_chars "+*?|" +bracket_chars "([])")
(defmacro is-op-char? (_) (list 'find _ +op_chars))
(defmacro is-esc-char? (_) (list 'find _ "\\"))
(defmacro is-bracket-char? (_) (list 'find _ +bracket_chars))

(defun dot? () (when dot (push out_types :op) (push out_tokens ".")))
(defun output (op token) (push out_types op) (push out_tokens token))

(defun tokenize (line)
	(defq state :space dot :nil out_types (clear '()) out_tokens (clear '()))
	(each (lambda (c)
		(while c
			(case state
				(:space
					(cond
						((is-esc-char? c)
							(dot?)
							(setq state :esc))
						((is-op-char? c)
							(output :op c)
							(setq dot :nil))
						((defq _ (is-bracket-char? c))
							(if (< _ 2) (dot?))
							(output (elem-get _ '(:lrb :lsb :rsb :rrb)) c)
							(setq dot (elem-get _ '(:nil :nil :t :t))))
						(:t	(dot?)
							(output :lit c)
							(setq dot :t)))
					(setq c :nil))
				(:esc
					(output :lit c)
					(setq c :nil dot :t state :space))))) line)
	(list out_tokens out_types))

(defun polish (tokenize_output)
	(defq token_stack '() out_tokens (clear '()) out_types (clear '()))
	(each! 0 -1 (lambda (token type)
		(case type
			((:lrb :lsb)
				(push token_stack token))
			(:rrb
				(while (and (not (eql "(" (setq token (pop token_stack)))) token)
					(output :op token)))
			(:rsb
				(while (and (not (eql "[" (setq token (pop token_stack)))) token)
					(output :op token))
				(output :op "[]"))
			(:op
				;precidence
				(defq p (find token +op_presidence) x :t)
				(while (and x (/= 0 (length token_stack)))
					(defq ps (find (elem-get -2 token_stack) +op_presidence))
					(cond
						((>= p ps)
							(output :op (pop token_stack)))
						(:t	(setq x :nil))))
				(push token_stack token))
			(:t	;lit
				(output type token)))) tokenize_output)
	(while (defq _ (pop token_stack))
		(output :op _))
	(list out_tokens out_types))

(defun compile (stack rpn_output)
	(each! 0 -1 (lambda (token type)
		(case type
			(:lit
				(push stack `(((,eql s ,token) 1))))
			(:op
				(case token
					("."
						(defq a (pop stack) b (pop stack))
						(push stack (cat b a)))))))
		rpn_output)
	(pop stack))

(defun addstate (creg next index state)
	(defq stack (slice 1 -1 state))
	(while (defq offset (pop stack))
		(cond
			((= (defq new_index (+ index offset)) (length creg))
				;end state
				(unless (find new_index next) (push next new_index)))
			(:t	(if (elem-get 0 (defq state (elem-get new_index creg)))
					;not a split
					(unless (find new_index next) (push next new_index))
					;a split
					(each! 1 -1 (# (push stack (+ offset %0))) (list state))))))
	next)

(defun firststate (creg current state)
	(if (elem-get 0 state)
		;not a split
		(push current 0)
		;a split
		(addstate creg current 0 state)))

(defun match (text creg start)
	; (match text creg start) -> :nil | end
	;for each char in text
	(and (defq current (firststate creg (list) (elem-get 0 creg)) next (list)
		end (some! start -1 :nil
			(lambda (s)
				;for each current state
				(each (lambda (index)
						(defq state (elem-get index creg))
						(when (eval (elem-get 0 state))
							;matched text char, so insert next states
							(addstate creg next index state)))
					current)
				;swap to new states
				(defq tmp current)
				(setq current next next (clear tmp))
				(cond
					;break out if no states
					((= (length current) 0) -1)
					;break out if any end states
					((find (length creg) current) (inc _))))
			(list text))) (> end -1) end))

(defclass Regexp () :nil
	; (Regexp) -> regexp
	(def this :meta_cache (fmap))

	(defmethod :compile (this pattern)
		; (. regexp :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq creg (. meta_cache :find pattern))
			(defq creg (if (= (length pattern) 0)
				:nil (compile (list) (polish (tokenize pattern)))))
			(. meta_cache :insert pattern creg))
		creg)

	(defmethod :search (this text pattern &optional creg)
		; (. regexp :search text pattern &optional meta) -> indexes
		(defq out (list) creg (opt creg (. this :compile pattern)))
		(when creg
			(defq x -1)
			(while (< (setq x (inc x)) (length text))
				(when (defq x1 (match text creg x))
					(push out (list x x1))
					(setq x (dec x1)))))
		out)
	)

;module
(export-classes
	Regexp)
;; (export-symbols
;; 	tokenize polish compile firststate addstate)
(env-pop)
