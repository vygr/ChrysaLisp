;;;;;;;;;;;;;;;;;;;;;
; Run length encoding
;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun flush_literals (out_stream write_pool_state literals token_bits run_bits)
	(when (nempty? literals)
		; Control token MSB is 0 for literal run. Value is length-1.
		(write-bits out_stream write_pool_state (dec (length literals)) run_bits)
		(each (# (write-bits out_stream write_pool_state %0 token_bits)) literals)
		(clear literals)))

(defun rle-compress (in_stream out_stream &optional token_bits run_bits)
	; (rle-compress in_stream out_stream [token_bits run_bits])
	(setd token_bits 8 run_bits 8)
	(defq read_pool_state (array 0 0)   ; State for the input stream
		write_pool_state (array 0 0)  ; State for the output stream
		sign_bit_mask (<< 1 (dec run_bits))
		length_mask (dec sign_bit_mask)
		max_run_length (inc length_mask)
		max_literals (inc length_mask)
		literals (list) ; list to prevent object re-allocation thrashing
		token (read-bits in_stream read_pool_state token_bits))
	(while (/= token -1)
		; Peek ahead to find run length of identical tokens
		(defq run_length 1 next_token (read-bits in_stream read_pool_state token_bits))
		(while (and (< run_length max_run_length) (eql next_token token))
			(++ run_length)
			(setq next_token (read-bits in_stream read_pool_state token_bits)))
		(if (>= run_length 2)
			(progn
				; End of a literal run, flush it
				(flush_literals out_stream write_pool_state literals token_bits run_bits)
				; Write the repeated run: control token and the token itself
				(write-bits out_stream write_pool_state (logior (dec run_length) sign_bit_mask) run_bits)
				(write-bits out_stream write_pool_state token token_bits))
			(progn
				; Not a long enough run, treat as literal
				(push literals token)
				(when (= (length literals) max_literals)
					(flush_literals out_stream write_pool_state literals token_bits run_bits))))
		; Slide the window forward
		(setq token next_token))
	; After loop, flush any remaining literals
	(flush_literals out_stream write_pool_state literals token_bits run_bits)
	; Flush any partial byte at the end of the output stream
	(flush-bits out_stream write_pool_state))

(defun rle-decompress (in_stream out_stream &optional token_bits run_bits max_tokens)
	; (rle-decompress in_stream out_stream [token_bits run_bits max_tokens])
	(setd token_bits 8 run_bits 8 max_tokens +max_long)
	(defq read_pool_state (array 0 0)   ; State for the input stream
		write_pool_state (array 0 0)  ; State for the output stream
		sign_bit_mask (<< 1 (dec run_bits))
		length_mask (dec sign_bit_mask))
	; Read breaking gracefully on -1 (EOF) or hitting token limit
	(while (and (> max_tokens 0)
				(/= -1 (defq len (read-bits in_stream read_pool_state run_bits))))
		(task-slice)
		(cond
			((bits? len sign_bit_mask)
				; Repeated Run (MSB is set)
				(setq max_tokens (- max_tokens
					(defq token (read-bits in_stream read_pool_state token_bits)
						len (logand len length_mask)) 1))
				(while (>= (-- len) -1)
					(write-bits out_stream write_pool_state token token_bits)))
			(:t ; Literal Run (MSB is clear)
				(setq max_tokens (- max_tokens len 1))
				(while (>= (-- len) -1)
					(write-bits out_stream write_pool_state (read-bits in_stream read_pool_state token_bits) token_bits)))))
	; Flush any partial byte at the end of the output stream
	(flush-bits out_stream write_pool_state))

;module
(export-symbols '(rle-compress rle-decompress))
(env-pop)
