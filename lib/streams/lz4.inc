;;;;;;;;;;;;;;;;;;;;;;
; LZ4 Fast Compression
;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +lz4_magic 0x184D2204)

; 13 bits = 8192 slots
(defmacro lz4-hash (v) `(logand (hash ,v) 8191))

(defun lz4-compress-raw (chunk hash_table)
	; (lz4-compress-raw chunk hash_table) -> string
	; Compresses a single independent raw LZ4 block.
	(defq i 0 anchor 0 c_len (length chunk)
		tok_buf (str-alloc (+ c_len 65536)) ; Safe upper bound for tokens
		t_pos 0 indices (nums))

	; Stop looking for matches 12 bytes before the end.
	; The final 5 bytes of a block must be literals.
	(while (< i (- c_len 12))
		(task-slice)
		; Read 4 bytes natively as an unsigned 32-bit int
		(defq val (get-uint chunk i) h (lz4-hash val)
			m_pos (elem-get hash_table h))
		(elem-set hash_table h i)

		(cond
			((and (>= m_pos 0)
					(<= (- i m_pos) 65535)
					(= val (get-uint chunk m_pos)))
				; --- MATCH FOUND ---
				; 1. Find match length
				(defq m_len 4)
				(some! (# (if (eql %0 (elem-get chunk (+ m_pos m_len))) (++ m_len)))
					(list chunk) :t (+ i m_len) (- c_len 5))

				; 2. Calculate Token
				(defq lit_len (- i anchor)
					token_lit (if (>= lit_len 15) 15 lit_len)
					token_match (if (>= (- m_len 4) 15) 15 (- m_len 4))
					token (logior (<< token_lit 4) token_match)
					t_start t_pos)

				; Write Token
				(set-byte tok_buf t_pos token) (++ t_pos)

				; Write Literal Length Extras
				(when (>= lit_len 15)
					(defq rem (- lit_len 15))
					(while (>= rem 255)
						(set-byte tok_buf t_pos 255) (++ t_pos)
						(-- rem 255))
					(set-byte tok_buf t_pos rem) (++ t_pos))

				; Write Offset (16-bit LE)
				(defq t_mid t_pos offset (- i m_pos))
				(set-short tok_buf t_pos offset) (++ t_pos 2)

				; Write Match Length Extras
				(when (>= (- m_len 4) 15)
					(defq rem (- m_len 19))
					(while (>= rem 255)
						(set-byte tok_buf t_pos 255) (++ t_pos)
						(-- rem 255))
					(set-byte tok_buf t_pos rem) (++ t_pos))

				; Splice strictly alternates between chunk and tok_buf.
				; (Empty chunk) -> (Token + Lit Extra) -> (Literals) -> (Offset + Match Extra)
				(push indices 0 0 t_start t_mid anchor i t_mid (defq t_end t_pos))

				; Fast forward over the match
				(setq i (+ i m_len) anchor i))
			((++ i))))

	; --- FINAL SEQUENCE ---
	; Encode the remaining bytes as a pure literal sequence
	(defq lit_len (- c_len anchor)
		token_lit (if (>= lit_len 15) 15 lit_len)
		t_start t_pos)

	(set-byte tok_buf t_pos (<< token_lit 4)) (++ t_pos)

	(when (>= lit_len 15)
		(defq rem (- lit_len 15))
		(while (>= rem 255)
			(set-byte tok_buf t_pos 255) (++ t_pos)
			(-- rem 255))
		(set-byte tok_buf t_pos rem) (++ t_pos))
	
	; Final append: (Empty chunk) -> (Final Token + Extras) -> (Final Literals)
	(push indices 0 0 t_start (defq t_mid t_pos) anchor c_len)

	; Perform the atomic slice/concat!
	(splice chunk tok_buf indices))

(defun lz4-decode-raw (chunk window max_block_size)
	; (lz4-decode-raw chunk window max_block_size) -> string
	; Decodes a raw LZ4 block, resolving overlaps smoothly.

	; Safely allocate using the actual Max Block Size
	(defq dec_block (str-alloc max_block_size)
		c_pos 0 c_len (length chunk) d_pos 0 w_len (length window))

	(while (< c_pos c_len)
		(task-slice)
		(defq token (get-ubyte chunk c_pos))
		(++ c_pos)

		; 1. Process Literals
		(defq lit_len (>> token 4))
		(when (= lit_len 15)
			(defq v 255)
			(while (= v 255)
				(setq v (get-ubyte chunk c_pos))
				(++ c_pos)
				(++ lit_len v)))

		(when (> lit_len 0)
			(defq end_c (+ c_pos lit_len))
			(while (< c_pos end_c)
				(set-byte dec_block d_pos (get-ubyte chunk c_pos))
				(++ c_pos) (++ d_pos)))

		; 2. Process Match (if not at EOF)
		(when (< c_pos c_len)
			(defq offset (logior
							(get-ubyte chunk c_pos)
							(<< (get-ubyte chunk (++ c_pos)) 8)))
			(++ c_pos)

			(defq match_len (logand token 0x0F))
			(when (= match_len 15)
				(defq v 255)
				(while (= v 255)
					(setq v (get-ubyte chunk c_pos))
					(++ c_pos)
					(++ match_len v)))
			(++ match_len 4)

			(defq end_d (+ d_pos match_len))

			; Read byte-by-byte to inherently resolve overlaps!
			(if (<= offset d_pos)
				; Reading internally from current block
				(progn
					(defq m_pos (- d_pos offset))
					(while (< d_pos end_d)
						(set-byte dec_block d_pos (get-ubyte dec_block m_pos))
						(++ d_pos) (++ m_pos)))
				; Reading back into sliding window
				(progn
					(defq m_pos (- w_len (- offset d_pos)))
					(while (< d_pos end_d)
						(if (< m_pos w_len)
							(set-byte dec_block d_pos (get-ubyte window m_pos))
							; Crossed the boundary back into current block
							(set-byte dec_block d_pos (get-ubyte dec_block (- m_pos w_len))))
						(++ d_pos) (++ m_pos))))))
	
	; Return exact length slice of decoded data
	(slice dec_block 0 d_pos))

(defun lz4-compress (in_stream out_stream &optional window_size)
	; (lz4-compress in_stream out_stream [window_size])
	(setd window_size 65536)
	(defq hash_table (cap 8192 (list)))
	(times 8192 (push hash_table -65536))
	;standard LZ4 Frame Header
	;FLG=0x60 (V1, Block Indep), BD=0x40 (64KB Max) -> HC=0x82
	(write-int out_stream +lz4_magic)
	(write-byte out_stream '(0x60 0x40 0x82))
	(while (defq chunk (read-blk in_stream window_size))
		(task-slice)
		;reset hash table
		(clear hash_table)
		(times 8192 (push hash_table -65536))
		(defq comp (lz4-compress-raw chunk hash_table))
		(if (>= (length comp) (length chunk))
			(progn
				;highest bit set indicates uncompressed
				(write-int out_stream (logior 0x80000000 (length chunk)))
				(write-blk out_stream chunk))
			(progn
				;output compressed length
				(write-int out_stream (length comp))
				(write-blk out_stream comp)))
		(setq chunk (read-blk in_stream window_size)))
	;write 0-byte EndMark
	(write-int out_stream 0))

(defun lz4-decompress (in_stream out_stream &optional window_size)
	; (lz4-decompress in_stream out_stream [window_size])
	(setd window_size 65536)
	(when (and (defq magic (read-int in_stream)) (= magic +lz4_magic))
		;calculate actual Max Block Size from the BD byte per LZ4 spec
		;frame defines independent blocks or sliding window
		(defq flg (read-byte in_stream) bd (read-byte in_stream) hc (read-byte in_stream)
			max_block_size (<< 1 (+ 8 (* 2 (logand (>> bd 4) 7))))
			block_indep (bits? flg 0x20) window "" running :t)
		(while running
			(task-slice)
			(if (or (not (defq size (read-uint in_stream))) (= size 0))
				;EndMark or EOF detected
				(setq running :nil)
				(progn
					(defq chunk (read-blk in_stream (logand size 0x7FFFFFFF)))
					(ifn (bits? size 0x80000000)
						;compressed block - pass max_block_size for safe allocation!
						(setq chunk (lz4-decode-raw chunk window max_block_size)))
					(write-blk out_stream chunk)
					;safely append and truncate sliding window circular buffer (ZERO WASTE)
					(setq window (cond
						(block_indep window)
						((<= (+ (length window) (length chunk)) window_size)
							;window hasn't wrapped yet
							(cat window chunk))
						((>= (length chunk) window_size)
							;chunk is huge, we don't need the old window at all
							(slice chunk (neg window_size) -1))
						;splice old window tail + new chunk into exactly 1 allocation
						((splice window chunk (nums (- (length chunk) window_size) -1 0 -1))))))))))

;module
(export-symbols '(lz4-compress lz4-decompress lz4-compress-raw lz4-decode-raw))
(env-pop)