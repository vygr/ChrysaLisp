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
		; Peek ahead to find run length of identical tokens (no lookahead list needed!)
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

	; Write a special, unused control code to unambiguously mark the End-Of-Stream.
	; This is the value for a "repeat run of length 1", which is not otherwise generated.
	(write-bits out_stream write_pool_state sign_bit_mask run_bits)

	; Flush any partial byte at the end of the output stream
	(flush-bits out_stream write_pool_state))

(defun rle-decompress (in_stream out_stream &optional token_bits run_bits)
	; (rle-decompress in_stream out_stream [token_bits run_bits])
	(setd token_bits 8 run_bits 8)
	(defq read_pool_state (array 0 0)   ; State for the input stream
		write_pool_state (array 0 0)  ; State for the output stream
		sign_bit_mask (<< 1 (dec run_bits))
		length_mask (dec sign_bit_mask)
		control_token 0)

	; Read directly into the while condition, breaking gracefully on -1 (EOF) or the EOS token
	(while (and (/= -1 (setq control_token (read-bits in_stream read_pool_state run_bits)))
				(/= control_token sign_bit_mask))
		(cond
			((bits? control_token sign_bit_mask)
				; Repeated Run (MSB is set)
				(defq run_length (inc (logand control_token length_mask))
					token_to_repeat (read-bits in_stream read_pool_state token_bits))
				; Fast integer loop
				(while (>= (-- run_length) 0)
					(write-bits out_stream write_pool_state token_to_repeat token_bits)))
			(:t	; Literal Run (MSB is clear)
				(defq literal_length (inc control_token) literal_token 0)
				(while (and (>= (-- literal_length) 0) 
							(/= -1 (setq literal_token (read-bits in_stream read_pool_state token_bits))))
					(write-bits out_stream write_pool_state literal_token token_bits)))))

	; Flush any partial byte at the end of the output stream
	(flush-bits out_stream write_pool_state))

;module
(export-symbols '(rle-compress rle-decompress))
(env-pop)