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
    (setd token_bits 8)
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

(defun huffman-decompress (in_stream out_stream &optional token_bits)
    ; (huffman-decompress in_stream out_stream [token_bits])
    (setd token_bits 8)
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
	; Writes a self-describing codebook (via the frequency map) to a stream.
	(defq write_pool_state (array 0 0)
		  entries (list))

	(. freq_map :each (# (push entries (list %0 %1))))

	; Write header: source token_bits (8 bits), Number of entries (32 bits).
	(write-bits out_stream write_pool_state token_bits 8)
	(write-bits out_stream write_pool_state (length entries) 32)

	; Write each entry.
	(each (lambda ((token freq))
        ; FIX: Write the token with a fixed 16-bit width to avoid truncation.
		(write-bits out_stream write_pool_state token 16)
		(write-bits out_stream write_pool_state freq 32)) entries)

	(flush-bits out_stream write_pool_state))

(defun huffman-read-codebook (in_stream)
	; Reads a self-describing codebook from a stream and reconstructs the model.
	(defq read_pool_state (array 0 0)
		  freq_map (Fmap 101))

	; Read header: token_bits for data, and number of entries.
	(defq token_bits  (read-bits in_stream read_pool_state 8)
		  num_entries (read-bits in_stream read_pool_state 32))

	; Read each entry from the stream and rebuild the frequency map.
	(times num_entries
        ; FIX: Read the token with a fixed 16-bit width to match the writer.
		(defq token (read-bits in_stream read_pool_state 16)
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

(defun test-static-huffman (test_string)
    (print "---")
    (print "Testing with string: " test_string)

    ; --- 1. Build the model from the original data ---
    (defq in_stream (string-stream test_string))
    (defq freq_map (huffman-build-freq-map in_stream 8))
    (bind '(root codebook) (build_tree_and_codebook freq_map))
    (defq original_model (list root codebook 8))

    ; --- 2. Save the model to a stream ---
    (defq model_stream (string-stream ""))
    (huffman-write-codebook model_stream 8 freq_map)
    (defq saved_model_string (str model_stream))

    ; --- 3. Load the model back from the saved string ---
    (defq load_model_stream (string-stream saved_model_string))
    (defq loaded_model (huffman-read-codebook load_model_stream))

    ; --- 4. Compress the original string using the loaded model ---
    (setq in_stream (string-stream test_string)) ; Rewind the input stream
    (defq compressed_stream (string-stream ""))
    (huffman-compress-static in_stream compressed_stream loaded_model)
    (defq compressed_string (str compressed_stream))
    (print "Compressed (encoded): " (id-encode compressed_string))

    ; --- 5. Decompress the data using the loaded model ---
    (defq decompress_in_stream (string-stream compressed_string))
    (defq decompressed_stream (string-stream ""))
    (huffman-decompress-static decompress_in_stream decompressed_stream loaded_model)
    (defq decompressed_string (str decompressed_stream))
    (print "Decompressed:         " decompressed_string)

    ; --- 6. Verify the result ---
    (if (eql test_string decompressed_string)
        (print "Status:               PASS")
        (print "Status:               FAIL")))

(defun test-static ()
    (test-static-huffman "abcdefjhiiijklmnop")
    (test-static-huffman "abcdefghijklmnop")
    (test-static-huffman "aaaaaaaaaa")
    (test-static-huffman "abcfcba")
    (test-static-huffman "Furthermore"))

(defun deep-eql (a b)
  "Recursively compares two nested lists for deep equality."
  (cond
    ((and (list? a) (list? b))
      (and (= (length a) (length b))
           ; FIX: Check if all mapped results are :t by ensuring :nil is not found.
           (not (find :nil (map deep-eql a b)))))
    (:t (eql a b))))

(defun fmap-to-sorted-list (fmap)
  "Converts an Fmap to a sorted list of (key value) pairs for comparison."
  (defq out (list))
  (. fmap :each (# (push out (list %0 %1))))
  (sort out (# (cmp (str (first %0)) (str (first %1))))))

(defun verify-model-integrity (test_string)
  "Builds, serializes, deserializes, and verifies a Huffman model."
  (print "---")
  (print "Verifying model integrity for string: " test_string)

  ; --- 1. Build the initial model ---
  (defq freq_map (huffman-build-freq-map (string-stream test_string) 8))
  (bind '(root1 codebook1) (build_tree_and_codebook freq_map))

  ; --- 2. Serialize the frequency map ---
  (defq model_stream (string-stream ""))
  (huffman-write-codebook model_stream 8 freq_map)

  ; --- 3. Deserialize to build the second model ---
  (defq load_model_stream (string-stream (str model_stream)))
  (bind '(root2 codebook2 _) (huffman-read-codebook load_model_stream))

  ; --- 4. Compare the two models ---
  (defq tree_match (deep-eql root1 root2))
  (defq codebook_match (deep-eql (fmap-to-sorted-list codebook1)
                                 (fmap-to-sorted-list codebook2)))

  (print "Tree structure match: " (if tree_match "PASS" "FAIL"))
  (print "Codebook content match: " (if codebook_match "PASS" "FAIL"))

  (if (and tree_match codebook_match)
      (print "Model Integrity Status: PASS")
      (progn
        (print "Model Integrity Status: FAIL")
        (print "--- Original Codebook ---")
        (. codebook1 :each print)
        (print "--- Loaded Codebook ---")
        (. codebook2 :each print)))
  tree_match)

(defun test-model ()
    (verify-model-integrity "abcdefjhiiijklmnop")
    (verify-model-integrity "aaaaaaaaaa")
    (verify-model-integrity "abcfcba"))
