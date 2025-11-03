;;;;;;;;;;;;;;;;
; Huffman coding
;;;;;;;;;;;;;;;;

;module
(env-push)

(defun build_tree_and_codebook (freq_map)
    ; Builds a deterministic, canonical Huffman tree and codebook from a frequency map.
    (defq nodes (list))

    ; Step 1: Create initial leaf nodes and assign a stable, sequential ID for deterministic sorting.
    ; Node Structure: (frequency token id)
    (. freq_map :each (# (push nodes (list %1 %0))))
    (sort nodes (# (- (second %0) (second %1))))
    (each (# (push %0 (!))) nodes)

    (defq codebook (Fmap (+ 2 (length nodes))))

    ; Step 2: Build the tree by repeatedly merging the two highest-priority nodes.
    (while (> (length nodes) 1)
        ; Sort DESCENDING by frequency, then DESCENDING by ID. This is a stable sort that
        ; places the two highest-priority nodes (lowest frequency, then lowest ID) at the end for popping.
        (sort nodes (# (if (/= (defq f (- (first %1) (first %0))) 0)
                           f
                           (- (last %1) (last %0)))))

        (defq n1 (pop nodes)  ; Highest priority node (lowest rank).
              n2 (pop nodes))  ; Second-highest priority node.

        ; For a canonical tree, the higher priority node (n1) is the '0' branch.
        ; The new internal node's ID is inherited from its highest-priority child (n1).
        ; Internal Node Structure: (frequency child1 child2 id)
        (push nodes (list (+ (first n1) (first n2)) n1 n2 (last n1))))

    ; Step 3: Iteratively traverse the final tree to build the codebook with pre-reversed bits.
    (when (defq root (first nodes))
        (defq stack (list (list root 0 0))) ; Stack: (node path length)
        (while (nempty? stack)
            (bind '(node path len) (pop stack))
            (if (> (length node) 3) ; It's an internal node: (frequency child0 child1 id)
                (push stack
                    ;; OPTIMIZED: This is the new pre-reversing logic.
                    ; '0' branch (left): The path value does not change, only the length increases.
                    (list (second node) path (inc len))
                    ; '1' branch (right): A '1' is added at the current most significant position.
                    (list (third node) (logior path (<< 1 len)) (inc len)))
                ; It's a leaf node: (frequency token id)
                (. codebook :insert (second node) (list path len)))))
    (list root codebook))


;;;;;;;;;;;;;;;;;
; Static Huffman
;;;;;;;;;;;;;;;;;

(defun huffman-build-freq-map (in_stream &optional token_bits)
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

	; Write each entry. The token is written with a fixed 16-bit width
	; to handle tokens larger than a byte (like the EOS token).
	(each (lambda ((token freq))
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
		(defq token (read-bits in_stream read_pool_state 16)
			  freq  (read-bits in_stream read_pool_state 32))
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
            ;; REMOVED: The call to reverse-bits is no longer needed.
			(write-bits out_stream write_pool_state code len)))

	; Write the End Of Stream token.
	(when (defq code_info (. codebook :find EOS_TOKEN))
		(bind '(code len) code_info)
        ;; REMOVED: The call to reverse-bits is no longer needed.
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
                ; The tree structure remains the same: n1 is the '0' branch, n2 is the '1' branch.
                (setq current_node (if (= bit 1) (third current_node) (second current_node)))))

        (when running
            (defq decoded_token (second current_node))
            (if (eql decoded_token EOS_TOKEN)
                (setq running :nil)
                (write-bits out_stream write_pool_state decoded_token token_bits))))

    (flush-bits out_stream write_pool_state))

;module
(export-symbols '(build_tree_and_codebook huffman-build-freq-map
	huffman-write-codebook huffman-read-codebook
	huffman-compress-static huffman-decompress-static))
(env-pop)

;;;
;;; TEST HARNESS (INSTRUMENTED)
;;;

(defun deep-eql (a b)
  (cond
    ((and (list? a) (list? b))
      (and (= (length a) (length b))
           (not (find :nil (map deep-eql a b)))))
    (:t (eql a b))))

(defun fmap-to-sorted-list (fmap)
  (defq out (list))
  (. fmap :each (# (push out (list %0 %1))))
  (sort out (# (cmp (str (first %0)) (str (first %1))))))

(defun print-codebook (cb)
  (each (lambda (entry) (print "  " (char (first entry)) " (" (first entry) ") -> " (second entry)))
        (fmap-to-sorted-list cb)))

(defun test-bitstream-integrity ()
  (print "---")
  (print "TEST 1: Verifying low-level bitstream functions...")
  (defq original_values (list)
        original_widths (list)
        write_pool_state (array 0 0)
        out_stream (string-stream "")
        num_tests 500
        seed 12345
        width 1)
  (times num_tests
    (setq seed (% (+ (* seed 1103515245) 12345) 0x7FFFFFFF)
          width (inc (% width 16)))
    (defq value (% seed (<< 1 width)))
    (push original_values value)
    (push original_widths width)
    (write-bits out_stream write_pool_state value width))
  (flush-bits out_stream write_pool_state)
  (defq read_values (list)
        read_pool_state (array 0 0)
        in_stream (string-stream (str out_stream)))
  (each (lambda (w) (push read_values (read-bits in_stream read_pool_state w))) original_widths)
  (if (deep-eql original_values read_values)
      (progn (print "  Status: PASS") :t)
      (progn (print "  Status: FAIL") (print "  Original:" original_values) (print "  Read:    " read_values) :nil)))

(defun test-model-determinism (test_string)
  (print "---")
  (print "TEST 2: Verifying model determinism for string: " test_string)
  (defq perms (list
                (map identity test_string)
                (reverse (map identity test_string))
                (sort (map identity test_string))
                (shuffle (map identity test_string)))
        perm_names '("Original" "Reversed" "Sorted" "Shuffled")
        models (list)
        EOS_TOKEN (inc (<< 1 8)))

  (each (lambda (perm)
    (defq freq_map (scatter (Fmap 101) EOS_TOKEN 1))
    (each (lambda (c)
      (. freq_map :update (code c) (# (if %0 (inc %0) 1)))) perm)
    (push models (build_tree_and_codebook freq_map)))
    perms)

  (defq golden_model (first models)
        all_match :t)

  (each! (lambda (perm_name perm current_model)
    (defq match (and (deep-eql (first golden_model) (first current_model))
                     (deep-eql (fmap-to-sorted-list (second golden_model))
                               (fmap-to-sorted-list (second current_model)))))
    (print "  - Permutation: " perm_name " (" (if (empty? perm) "" (apply cat perm)) ") ... " (if match "PASS" "FAIL"))
    (unless match (setq all_match :nil)))
    (list (rest perm_names) (rest perms) (rest models)))

  (if all_match
      (progn (print "  Determinism Status: PASS") :t)
      (progn (print "  Determinism Status: FAIL") :nil)))

(defun verify-model-integrity (test_string)
  (print "---")
  (print "TEST 3: Verifying model serialization for string: " test_string)
  (defq freq_map (huffman-build-freq-map (string-stream test_string) 8))
  (bind '(root1 codebook1) (build_tree_and_codebook freq_map))
  (defq model_stream (string-stream ""))
  (huffman-write-codebook model_stream 8 freq_map)
  (bind '(root2 codebook2 _) (huffman-read-codebook (string-stream (str model_stream))))
  (defq tree_match (deep-eql root1 root2))
  (defq codebook_match (deep-eql (fmap-to-sorted-list codebook1) (fmap-to-sorted-list codebook2)))
  (print "  Tree structure match:     " (if tree_match "PASS" "FAIL"))
  (print "  Codebook content match:   " (if codebook_match "PASS" "FAIL"))
  (if (and tree_match codebook_match)
      (progn (print "  Model Integrity Status:   PASS") :t)
      (progn (print "  Model Integrity Status:   FAIL") :nil)))

(defun test-static-compression (test_string)
  (print "---")
  (print "TEST 4: Verifying static end-to-end for string: " test_string)

  (if (eql test_string "abcfcba")
    (progn (print "[TRACE] Starting trace for 'abcfcba'") (def (env) '*trace-enabled* :t))
    (undef (env) '*trace-enabled*))

  (defq freq_map (huffman-build-freq-map (string-stream test_string) 8))
  (defq model_stream (string-stream ""))
  (huffman-write-codebook model_stream 8 freq_map)
  (defq model (huffman-read-codebook (string-stream (str model_stream))))

  (if (get '*trace-enabled* (env))
    (progn
      (print "[DECOMPRESS-MODEL] Codebook as seen by decompressor:")
      (print-codebook (second model))))

  (defq compressed_stream (string-stream ""))
  (huffman-compress-static (string-stream test_string) compressed_stream model)

  (defq decompressed_stream (string-stream ""))
  (huffman-decompress-static (string-stream (str compressed_stream)) decompressed_stream model)

  (defq result (str decompressed_stream))
  (if (eql test_string result)
      (progn (print "  Status: PASS") :t)
      (progn (print "  Status: FAIL") (print "  Expected: " test_string) (print "  Got:      " result) :nil)))

(defun run-all-huffman-tests ()
  (defq test_strings (list
    ""
    "a"
    "aaaaaaaaaa"
    "abcfcba"
    "abcdefghijklmnop"
    "abcdefjhiiijklmnop"
    "Furthermore"
  ))
  (print "=====================================")
  (print "       Starting Huffman Tests")
  (print "=====================================")
  (if (test-bitstream-integrity)
    (if (every (# (test-model-determinism %0)) test_strings)
      (if (every (# (verify-model-integrity %0)) test_strings)
        (if (every (# (test-static-compression %0)) test_strings)
			(progn
				(print "---")
				(print "ALL TESTS PASSED")
				(print "====================================="))
          (print "Static test failed."))
        (print "Model serialization test failed."))
      (print "Model determinism test failed."))
    (print "Bitstream test failed.")))