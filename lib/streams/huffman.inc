;;;;;;;;;;;;;;;;;;
; Adaptive Huffman
;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun build_tree_and_codebook (freq_map)
    ; Builds a deterministic Huffman tree and codebook from a frequency map.
    (defq nodes (list))

    ; Step 1: Create initial leaf nodes. Order is non-deterministic.
    ; Node Structure: (frequency token)
    (. freq_map :each (# (push nodes (list %1 %0))))

    ; Step 2: Create a canonical ordering by sorting the leaves by token value.
    (sort nodes (# (- (second %0) (second %1))))

    ; Step 3: Assign a stable, sequential ID to each node in-place.
    ; Node Structure becomes: (frequency token id)
    (each (# (push %0 (!))) nodes)

    (defq codebook (Fmap (+ 2 (length nodes))))

    ; Step 4: Build the tree by repeatedly merging the two lowest-frequency nodes.
    (while (> (length nodes) 1)
        ; Stable sort: DESCENDING by frequency, then DESCENDING by ID.
        (sort nodes (# (if (/= (defq f (- (first %1) (first %0))) 0)
                           f
                           (- (last %1) (last %0)))))

        (defq n1 (pop nodes)
              n2 (pop nodes))

        ; New internal node inherits the ID of the lowest-ranked child (n1).
        ; Internal Node Structure: (frequency child1 child2 id)
        (push nodes (list (+ (first n1) (first n2)) n1 n2 (last n1))))

    ; Step 5: Iteratively build the codebook from the final tree.
    (when (defq root (first nodes))
        (defq stack (list (list root 0 0))) ; Stack: (node path length)
        (while (nempty? stack)
            (bind '(node path len) (pop stack))
            (if (> (length node) 3)
                (push stack
                    (list (second node) (<< path 1) (inc len))
                    (list (third node) (logior (<< path 1) 1) (inc len)))
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
				(write-bits out_stream write_pool_state code len)
				(. freq_map :update token (# (inc %0))))
			(progn ; Token is not yet seen (NYT).
				; Write the NYT code, followed by the raw token.
				(bind '(code len) (. codebook :find NYT_TOKEN))
				(write-bits out_stream write_pool_state code len)
				(write-bits out_stream write_pool_state token token_bits)
				; Update frequencies for BOTH the NYT token and the new token.
				(. freq_map :update NYT_TOKEN (# (inc %0)))
				(. freq_map :insert token 1))))

	; After the loop, build the final model and send the End Of Stream token.
	(bind '(_ codebook) (build_tree_and_codebook freq_map))
	(bind '(code len) (. codebook :find EOS_TOKEN))
	(write-bits out_stream write_pool_state code len)

	(flush-bits out_stream write_pool_state))

(defun huffman-decompress (in_stream out_stream &optional token_bits)
    ; (huffman-decompress in_stream out_stream [token_bits])
    ; adaptive single pass
    (setd token_bits 8)
    (defq read_pool_state (array 0 0)
          write_pool_state (array 0 0)
          NYT_TOKEN (<< 1 token_bits)
          EOS_TOKEN (inc NYT_TOKEN)
          freq_map (scatter (Fmap 101) NYT_TOKEN 1 EOS_TOKEN 1)
          running :t)

    (while running
        (bind '(root _) (build_tree_and_codebook freq_map))
        (defq current_node root)

        ; Traverse the tree one bit at a time to find the next token.
        ; The condition is changed from > 2 to > 3 to account for the new node format.
        (while (and running current_node (> (length current_node) 3))
            (defq bit (read-bits in_stream read_pool_state 1))
            (if (= bit -1)
                (setq running :nil)
                (setq current_node (if (= bit 1) (third current_node) (second current_node)))))

        (when running
            (defq decoded_token (second current_node))

            (cond
                ((eql decoded_token EOS_TOKEN)
                    (setq running :nil))
                ((eql decoded_token NYT_TOKEN)
                    (setq decoded_token (read-bits in_stream read_pool_state token_bits))
                    (if (= decoded_token -1)
                        (setq running :nil))))

            (when running
                (write-bits out_stream write_pool_state decoded_token token_bits)
                (. freq_map :update decoded_token (# (if %0 (inc %0) 1))))))

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
    (bind '(root _ token_bits) model)
    (defq read_pool_state (array 0 0)
          write_pool_state (array 0 0)
          EOS_TOKEN (inc (<< 1 token_bits))
          running :t)

    (while running
        (defq current_node root)
        ; Traverse the tree one bit at a time to find the next token.
        ; The condition is changed from > 2 to > 3 to account for the new node format.
        (while (and running current_node (> (length current_node) 3))
            (defq bit (read-bits in_stream read_pool_state 1))
            (if (= bit -1)
                (setq running :nil)
                (setq current_node (if (= bit 1) (third current_node) (second current_node)))))

        (when running
            (defq decoded_token (second current_node))
            (if (eql decoded_token EOS_TOKEN)
                (setq running :nil)
                (write-bits out_stream write_pool_state decoded_token token_bits))))

    (flush-bits out_stream write_pool_state))

;module
(export-symbols '(huffman-compress huffman-decompress build_tree_and_codebook
	huffman-build-freq-map huffman-write-codebook huffman-read-codebook
	huffman-compress-static huffman-decompress-static))
(env-pop)

(defun print-results (state)
	(bind '(tree cbook) state)
	(print "Tree")
	(print tree)
	(print "Codebook")
	(. cbook :each print))

(defun test-huffman ()
	(print-results (build_tree_and_codebook (scatter (Fmap) 1 1 2 2 3 3 4 4 0 5)))
	(print-results (build_tree_and_codebook (scatter (Fmap) 1 1 2 1 3 1 4 1 0 1)))
	(print-results (build_tree_and_codebook (scatter (Fmap) 2 1 3 1 4 1 0 1 1 1)))
	(print-results (build_tree_and_codebook (scatter (Fmap) 3 1 4 1 0 1 1 1 2 1))))
