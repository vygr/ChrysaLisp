;;;;;;;;;;;;;;;;;;;;;
; lib/streams/rle.inc
;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun flush_literals (out_stream write_pool_state literals token_bits run_bits)
    (when (nempty? literals)
        ; Control token MSB is 0 for literal run. Value is length-1.
        (write-bits out_stream write_pool_state (dec (length literals)) run_bits)
        (each (lambda (token) (write-bits out_stream write_pool_state token token_bits)) literals)
        (clear literals)))

(defun rle-compress (in_stream out_stream &optional token_bits run_bits)
    ; (rle-compress in_stream out_stream [token_bits run_bits])
    (setd token_bits 8 run_bits 8)
    (defq read_pool_state (array 0 0)   ; State for the input stream
          write_pool_state (array 0 0)  ; State for the output stream
          sign_bit_mask (<< 1 (dec run_bits))
          length_mask (dec sign_bit_mask)
          max_run_length (+ 2 length_mask)
          max_literals (inc length_mask)
          literals (list)
          lookahead (list))

    (while (/= -1 (defq token (if (nempty? lookahead)
                                  (pop lookahead)
                                  (read-bits in_stream read_pool_state token_bits))))

        ; Peek ahead to find run length of identical tokens
        (defq run_length 1)
        (while (and (< run_length max_run_length) (eql (defq next_token (read-bits in_stream read_pool_state token_bits)) token))
            (push lookahead next_token)
            (++ run_length))
        (unless (eql next_token -1) (push lookahead next_token))

        (if (>= run_length 2)
            (progn
                ; End of a literal run, flush it
                (flush_literals out_stream write_pool_state literals token_bits run_bits)

                ; Write the repeated run: control token and the token itself
                (defq encoded_length (logior (- run_length 1) sign_bit_mask))
                (write-bits out_stream write_pool_state encoded_length run_bits)
                (write-bits out_stream write_pool_state token token_bits)

                ; Consume the matching tokens from the lookahead buffer
                (setq lookahead (slice lookahead (dec run_length) -1)))
            (progn
                ; Not a long enough run, treat as literal
                (push literals token)
                (when (= (length literals) max_literals)
                    (flush_literals out_stream write_pool_state literals token_bits run_bits)))))

    ; After loop, flush any remaining literals and the bit pool
    (flush_literals out_stream write_pool_state literals token_bits run_bits)
    (flush-bits out_stream write_pool_state))

(defun rle-decompress (in_stream out_stream &optional token_bits run_bits)
    ; (rle-decompress in_stream out_stream [token_bits run_bits])
    (setd token_bits 8 run_bits 8)
    (defq read_pool_state (array 0 0)   ; State for the input stream
          write_pool_state (array 0 0)  ; State for the output stream
          sign_bit_mask (<< 1 (dec run_bits))
          length_mask (dec sign_bit_mask))

    (while (/= -1 (defq control_token (read-bits in_stream read_pool_state run_bits)))
        (if (bits? control_token sign_bit_mask)
            (progn
                ; Repeated Run (MSB is set)
                (defq run_length (inc (logand control_token length_mask)))
                (defq token_to_repeat (read-bits in_stream read_pool_state token_bits))
                (times run_length (write-bits out_stream write_pool_state token_to_repeat token_bits)))
            (progn
                ; Literal Run (MSB is clear)
                (defq literal_length (inc control_token))
                (times literal_length
                    (defq literal_token (read-bits in_stream read_pool_state token_bits))
                    (write-bits out_stream write_pool_state literal_token token_bits)))))
    
    ; Flush any partial byte at the end of the output stream
    (flush-bits out_stream write_pool_state))

;module
(export-symbols '(rle-compress rle-decompress))
(env-pop)
