;;;;;;;;;;;;;;;;;;
; Adaptive Huffman
;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun build_tree_and_codebook (freq_map)
	; Builds the Huffman tree and codebook from a frequency map. Non-recursive.
	(defq nodes (list))

	; Create initial leaf nodes.
	(. freq_map :each (# (push nodes (list %1 %0))))
	(defq codebook (Fmap (+ 2 (length nodes))))

	; Build the tree by repeatedly merging the two lowest-frequency nodes.
	(while (> (length nodes) 1)
		(sort nodes (# (- (first %1) (first %0))))
		(defq n1 (pop nodes) n2 (pop nodes))
		(push nodes (list (+ (first n1) (first n2)) n1 n2)))

	; Iteratively build the codebook from the tree using an explicit stack.
	(when (defq root (first nodes))
		; Stack holds (node path length)
		(defq stack (list (list root 0 0)))
		(while (nempty? stack)
			(bind '(node path len) (pop stack))
			(if (> (length node) 2)
				; Internal node: (frequency left_child right_child)
				(push stack
					(list (second node) (<< path 1) (inc len))
					(list (third node) (logior (<< path 1) 1) (inc len)))
				; Leaf node: (frequency token)
				(. codebook :insert (second node) (list path len)))))
	(list root codebook))

(defun huffman-compress (in_stream out_stream &optional token_bits)
	; (huffman-compress in_stream out_stream [token_bits])
	; adaptive single pass
	(setd token_bits 8)
	(defq read_pool_state (array 0 0)
		  write_pool_state (array 0 0)
		  ; Special tokens outside the normal token range
		  NYT_TOKEN (<< 1 token_bits)
		  EOS_TOKEN (inc NYT_TOKEN)
		  ; Start with NYT and EOS in the frequency map
		  freq_map (scatter (Fmap 101) NYT_TOKEN 1 EOS_TOKEN 1))

	(while (/= -1 (defq token (read-bits in_stream read_pool_state token_bits)))
		; Build the Huffman model based on frequencies seen so far.
		(bind '(_ codebook) (build_tree_and_codebook freq_map))

		(if (. codebook :find token)
			(progn ; Token is known, write its code.
				(bind '(code len) (. codebook :find token))
				(write-bits out_stream write_pool_state code len))
			(progn ; Token is not yet seen (NYT).
				; Write the NYT code, followed by the raw token.
				(bind '(code len) (. codebook :find NYT_TOKEN))
				(write-bits out_stream write_pool_state code len)
				(write-bits out_stream write_pool_state token token_bits)))

		; Update the frequency model for the token we just processed.
		(. freq_map :update token (# (if %0 (inc %0) 1))))

	; After the loop, build the final model and send the End Of Stream token.
	(bind '(_ codebook) (build_tree_and_codebook freq_map))
	(bind '(code len) (. codebook :find EOS_TOKEN))
	(write-bits out_stream write_pool_state code len)

	(flush-bits out_stream write_pool_state))

(defun huffman-decompress (in_stream out_stream &optional token_bits)
	; (huffman-decompress in_stream out_stream [token_bits])
	; adaptive single pass
	; This function is designed to be robust against premature end-of-stream conditions.
	(setd token_bits 8)
	(defq read_pool_state (array 0 0)
		  write_pool_state (array 0 0)
		  NYT_TOKEN (<< 1 token_bits)
		  EOS_TOKEN (inc NYT_TOKEN)
		  freq_map (scatter (Fmap 101) NYT_TOKEN 1 EOS_TOKEN 1)
		  running :t)

	(while running
		; Build the Huffman model to match the state of the compressor at this point.
		(bind '(root _) (build_tree_and_codebook freq_map))
		(defq current_node root)

		; Traverse the tree one bit at a time to find the next token.
		(while (and running current_node (> (length current_node) 2))
			(defq bit (read-bits in_stream read_pool_state 1))
			(if (= bit -1)
				(setq running :nil) ; Gracefully stop if stream ends mid-traversal.
				(setq current_node (if (= bit 1) (third current_node) (second current_node)))))

		; Only proceed if the main loop is still active.
		(when running
			(defq decoded_token (second current_node))

			(cond
				((eql decoded_token EOS_TOKEN)
					(setq running :nil)) ; Clean exit upon receiving the EOS token.
				((eql decoded_token NYT_TOKEN)
					; A new, unseen token is next in the stream. Read it raw.
					(setq decoded_token (read-bits in_stream read_pool_state token_bits))
					(if (= decoded_token -1)
						(setq running :nil)))) ; Gracefully stop if stream ends here.

			; If still running after potentially decoding a token...
			(when running
				; Write the decoded token to the output.
				(write-bits out_stream write_pool_state decoded_token token_bits)
				; Update the frequency model, keeping it in sync with the compressor.
				(. freq_map :update decoded_token (# (if %0 (inc %0) 1))))))

	; Flush any partial byte at the end of the output stream.
	(flush-bits out_stream write_pool_state))

;;;;;;;;;;;;;;;;;
; Static Huffman
;;;;;;;;;;;;;;;;;

(defun huffman-build-freq-map (in_stream &optional token_bits)
	; (huffman-build-freq-map in_stream [token_bits]) -> freq_map
	; Scans a stream to build a frequency map for static Huffman coding.
	(setd token_bits 8)
	(defq read_pool_state (array 0 0)
		  EOS_TOKEN (inc (<< 1 token_bits))
		  freq_map (scatter (Fmap 101) EOS_TOKEN 1))

	(while (/= -1 (defq token (read-bits in_stream read_pool_state token_bits)))
		(. freq_map :update token (# (if %0 (inc %0) 1))))
	freq_map)

(defun huffman-write-codebook (out_stream token_bits freq_map)
	; (huffman-write-codebook out_stream token_bits freq_map)
	; Writes a self-describing codebook (via the frequency map) to a stream.
	(defq write_pool_state (array 0 0)
		  entries (list))

	(. freq_map :each (# (push entries (list %0 %1))))

	; Write header: token_bits (8 bits), Number of entries (32 bits).
	(write-bits out_stream write_pool_state token_bits 8)
	(write-bits out_stream write_pool_state (length entries) 32)

	; Write each entry.
	(each (lambda ((token freq))
		(write-bits out_stream write_pool_state token token_bits)
		(write-bits out_stream write_pool_state freq 32)
	) entries)

	(flush-bits out_stream write_pool_state))

(defun huffman-read-codebook (in_stream)
	; (huffman-read-codebook in_stream) -> (list root codebook token_bits)
	; Reads a self-describing codebook from a stream and reconstructs the model.
	(defq read_pool_state (array 0 0)
		  freq_map (Fmap 101))

	; Read header: token_bits and number of entries.
	(defq token_bits  (read-bits in_stream read_pool_state 8)
		  num_entries (read-bits in_stream read_pool_state 32))

	; Read each entry from the stream and rebuild the frequency map.
	(times num_entries
		(defq token (read-bits in_stream read_pool_state token_bits)
			  freq  (read-bits in_stream read_pool_state 32))
		(. freq_map :insert token freq))

	; Build the tree and codebook from the reconstructed frequency map.
	(bind '(root codebook) (build_tree_and_codebook freq_map))
	(list root codebook token_bits))

(defun huffman-compress-static (in_stream out_stream model)
	; (huffman-compress-static in_stream out_stream model)
	; Compresses a stream using a pre-built static model.
	(bind '(_ codebook token_bits) model)
	(defq read_pool_state (array 0 0)
		  write_pool_state (array 0 0)
		  EOS_TOKEN (inc (<< 1 token_bits)))

	(while (/= -1 (defq token (read-bits in_stream read_pool_state token_bits)))
		(when (defq code_info (. codebook :find token))
			(bind '(code len) code_info)
			(write-bits out_stream write_pool_state code len)))

	; Write the End Of Stream token.
	(when (defq code_info (. codebook :find EOS_TOKEN))
		(bind '(code len) code_info)
		(write-bits out_stream write_pool_state code len))

	(flush-bits out_stream write_pool_state))

(defun huffman-decompress-static (in_stream out_stream model)
	; (huffman-decompress-static in_stream out_stream model)
	; Decompresses a stream using a pre-built static Huffman model.
	(bind '(root _ token_bits) model)
	(defq read_pool_state (array 0 0)
		  write_pool_state (array 0 0)
		  EOS_TOKEN (inc (<< 1 token_bits))
		  running :t)

	(while running
		(defq current_node root)
		; Traverse the tree one bit at a time to find the next token.
		(while (and running current_node (> (length current_node) 2))
			(defq bit (read-bits in_stream read_pool_state 1))
			(if (= bit -1)
				(setq running :nil) ; Gracefully stop if stream ends unexpectedly.
				(setq current_node (if (= bit 1) (third current_node) (second current_node)))))

		(when running
			(defq decoded_token (second current_node))
			(if (eql decoded_token EOS_TOKEN)
				(setq running :nil) ; Clean exit.
				(write-bits out_stream write_pool_state decoded_token token_bits))))

	(flush-bits out_stream write_pool_state))


;module
(export-symbols '(huffman-compress huffman-decompress
	huffman-build-freq-map huffman-write-codebook huffman-read-codebook
	huffman-compress-static huffman-decompress-static))
(env-pop)
