;;;;;;;;;;;;;;;;
; Huffman coding
;;;;;;;;;;;;;;;;

; this is purely experimental code to test the AI ! It is functionally correct,
; but that's about all you can say about it. ;)

; A true, high-performance adaptive Huffman implementation (like the Vitter
; algorithm) uses a much more complex tree data structure that allows for O(1) or
; O(log n) updates to the tree itself when frequencies change, rather than a full
; O(n log n) rebuild. Our current implementation is a correct simulation of an
; adaptive model but not a performant one.

; For real-world applications, using the static Huffman functions
; huffman-compress-static would be vastly faster, as it builds the tree only
; once after a preliminary scan of the file. This test perfectly illustrates the
; trade-offs between the simplicity of the adaptive simulation and the performance
; of a static model.

;module
(env-push)

(defun build_tree_and_codebook (freq_map)
	; Builds a deterministic, canonical Huffman tree and codebook from a frequency map.
	; This version uses a more optimal insertion sort pattern for tree building.
	(defq nodes (list))

	; Step 1: Create initial leaf nodes from the frequency map.
	; Node Structure: (frequency token)
	(. freq_map :each (# (push nodes (list %1 %0))))

	; Step 2: Create a canonical ordering by sorting the leaves by token value (ascending).
	(sort nodes (# (- (second %0) (second %1))))

	; Step 3: Assign a stable, sequential ID to each node in-place for deterministic tie-breaking.
	; Node Structure becomes: (frequency token id)
	(each (# (push %0 (!))) nodes)

	(defq codebook (Fmap (+ 2 (length nodes))))

	; Step 4: Perform an initial sort of the nodes by priority (frequency descending, then ID descending).
	; This prepares the list for the efficient pop-and-insert loop.
	(sort nodes (# (if (/= (defq f (- (first %1) (first %0))) 0)
								f
								(- (last %1) (last %0)))))

	; Step 5: Build the tree by repeatedly merging the two highest-priority (lowest frequency) nodes.
	(while (> (length nodes) 1)
		; The two nodes with the lowest frequency are now at the end of the list.
		(defq n1 (pop nodes)
			n2 (pop nodes))

		; Create the new internal node. It inherits the ID of the child with the lower ID (n1).
		(defq new_node (list (+ (first n1) (first n2)) n1 n2 (last n1)))

		; Find the correct position to insert the new node to maintain the descending sort order.
		(defq insert_pos (some! (# (if (> (first new_node) (first %0))
										(!)
											(if (and (= (first new_node) (first %0))
												(> (last new_node) (last %0)))
												(!))))
								(list nodes)))

		; Insert the new node at the found position, or push to the end if it's the new lowest-priority node.
		(if insert_pos
			(setq nodes (cat (slice nodes 0 insert_pos) (list new_node) (slice nodes insert_pos -1)))
			(push nodes new_node)))

	; Step 6: Iteratively build the codebook from the final tree.
	(when (defq root (first nodes))
		(defq stack (list (list root 0 0))) ; Stack: (node path length)
		(while (nempty? stack)
			(bind '(node path len) (pop stack))
			(if (> (length node) 3) ; It's an internal node
				(push stack
					(list (second node) path (inc len)) ; Child 1 gets a '0' bit
					(list (third node) (logior path (<< 1 len)) (inc len))) ; Child 2 gets a '1' bit
				; It's a leaf node
				(. codebook :insert (second node) (list path len)))))
	(list root codebook))

;;;;;;;;;;;;;;;;;;
; Adaptive Huffman
;;;;;;;;;;;;;;;;;;

(defun huffman-compress (in_stream out_stream token_bits)
	; (huffman-compress in_stream out_stream token_bits)
	(defq read_pool_state (array 0 0)
		write_pool_state (array 0 0)
		NYT_TOKEN (<< 1 token_bits)
		EOS_TOKEN (inc NYT_TOKEN)
		freq_map (scatter (Fmap 101) NYT_TOKEN 1 EOS_TOKEN 1))

	(while (/= -1 (defq token (read-bits in_stream read_pool_state token_bits)))
		(bind '(_ codebook) (build_tree_and_codebook freq_map))

		(if (defq code_info (. codebook :find token))
			; Token is known, write its code.
			(progn
				(bind '(code len) code_info)
				(write-bits out_stream write_pool_state code len))
			; Token is not yet seen (NYT).
			(progn
				(bind '(code len) (. codebook :find NYT_TOKEN))
				(write-bits out_stream write_pool_state code len)
				(write-bits out_stream write_pool_state token token_bits)))

		; Symmetrically update the frequency map AFTER processing the token.
		(. freq_map :update token (# (if %0 (inc %0) 1))))

	; Send the End Of Stream token based on the final model state.
	(bind '(_ codebook) (build_tree_and_codebook freq_map))
	(when (defq code_info (. codebook :find EOS_TOKEN))
		(bind '(code len) code_info)
		(write-bits out_stream write_pool_state code len))

	(flush-bits out_stream write_pool_state))

(defun huffman-decompress (in_stream out_stream token_bits)
	; (huffman-decompress in_stream out_stream token_bits)
	(defq read_pool_state (array 0 0)
		write_pool_state (array 0 0)
		NYT_TOKEN (<< 1 token_bits)
		EOS_TOKEN (inc NYT_TOKEN)
		freq_map (scatter (Fmap 101) NYT_TOKEN 1 EOS_TOKEN 1)
		running :t)

	(while running
		(bind '(root _) (build_tree_and_codebook freq_map))
		(defq current_node root token_to_process :nil)

		; Traverse the tree one bit at a time to find the next symbol.
		(while (and running current_node (> (length current_node) 3))
			(defq bit (read-bits in_stream read_pool_state 1))
			(if (= bit -1)
				(setq running :nil)
				(setq current_node (if (= bit 1) (third current_node) (second current_node)))))

		(when running
			; First, determine what the token is.
			(setq token_to_process (second current_node))

			(cond
				((eql token_to_process EOS_TOKEN)
					(setq running :nil))
				((eql token_to_process NYT_TOKEN)
					(setq token_to_process (read-bits in_stream read_pool_state token_bits))
					(if (= token_to_process -1) (setq running :nil))))

			; If we are still running, process the token and update the model.
			(when running
				(write-bits out_stream write_pool_state token_to_process token_bits)

				; Symmetrically update the frequency map AFTER processing the token.
				(. freq_map :update token_to_process (# (if %0 (inc %0) 1))))))

	(flush-bits out_stream write_pool_state))

;;;;;;;;;;;;;;;;;
; Static Huffman
;;;;;;;;;;;;;;;;;

(defun huffman-build-freq-map (in_stream token_bits)
	; Scans a stream to build a frequency map for static Huffman coding.
	(defq read_pool_state (array 0 0)
		EOS_TOKEN (inc (<< 1 token_bits))
		freq_map (scatter (Fmap 101) EOS_TOKEN 1))

	(while (/= -1 (defq token (read-bits in_stream read_pool_state token_bits)))
		(. freq_map :update token (# (if %0 (inc %0) 1)))
		(task-slice))
	freq_map)

(defun huffman-write-codebook (out_stream token_bits freq_map)
	; Writes a self-describing codebook (via the frequency map) to a stream.
	(defq write_pool_state (array 0 0)
		entries (list))

	(. freq_map :each (# (push entries (list %0 %1))))

	; Write header: source token_bits (8 bits), Number of entries (32 bits).
	(write-bits out_stream write_pool_state token_bits 8)
	(write-bits out_stream write_pool_state (length entries) 32)

	; Write each entry.
	(each (lambda ((token freq))
		(write-bits out_stream write_pool_state token 16)
		(write-bits out_stream write_pool_state freq 32)) entries)

	(flush-bits out_stream write_pool_state))

(defun huffman-read-codebook (in_stream)
	; Reads a self-describing codebook from a stream and reconstructs the model.
	(defq read_pool_state (array 0 0) freq_map (Fmap 101))

	; Read header: token_bits for data, and number of entries.
	(defq token_bits(read-bits in_stream read_pool_state 8)
		num_entries (read-bits in_stream read_pool_state 32))

	; Read each entry from the stream and rebuild the frequency map.
	(times num_entries
		(defq token (read-bits in_stream read_pool_state 16)
			freq(read-bits in_stream read_pool_state 32))
		(. freq_map :insert token freq))

	; Build the tree and codebook from the reconstructed frequency map.
	(bind '(root codebook) (build_tree_and_codebook freq_map))
	(list root codebook token_bits))

(defun huffman-compress-static (in_stream out_stream model)
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
	; Decompresses a stream using a pre-built static model.
	(bind '(root _ token_bits) model)
	(defq read_pool_state (array 0 0)
		write_pool_state (array 0 0)
		EOS_TOKEN (inc (<< 1 token_bits))
		running :t)

	(while running
		(defq current_node root)
		; Traverse the tree one bit at a time to find the next token.
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
(export-symbols '(build_tree_and_codebook huffman-compress huffman-decompress
	huffman-build-freq-map huffman-write-codebook huffman-read-codebook
	huffman-compress-static huffman-decompress-static))
(env-pop)
