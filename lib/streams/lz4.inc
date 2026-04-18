;;;;;;;;;;;;;;;;;;;;;;
; LZ4 Fast Compression
;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +lz4_magic 0x184D2204)

(defmacro lz4-hash (v)
	; 13 bits = 8192 slots. Constant 2654435761 spreads entropy.
	`(logand (>> (* ,v 2654435761) 19) 8191))

(defun lz4-compress-raw (chunk hash_table)
	; (lz4-compress-raw string nums_array) -> string
	; Compresses a single independent raw LZ4 block.
	(defq i 0 anchor 0 c_len (length chunk)
		  tok_buf (str-alloc (+ c_len 65536)) ; Safe upper bound for tokens
		  t_pos 0 indices (nums))

	; Stop looking for matches 12 bytes before the end.
	; The final 5 bytes of a block must be literals.
	(while (< i (- c_len 12))
		(task-slice)
		; Read 4 bytes natively as an unsigned 32-bit int
		(defq val (get-uint chunk i)
			  h (lz4-hash val)
			  m_pos (elem-get hash_table h))
		(elem-set hash_table h i)

		(cond
			((and (>= m_pos 0)
				  (<= (- i m_pos) 65535)
				  (= val (get-uint chunk m_pos)))
				; --- MATCH FOUND ---
				; 1. Find match length
				(defq m_len 4 limit (- c_len 5))
				(while (and (< (+ i m_len) limit)
							(= (get-ubyte chunk (+ i m_len))
							   (get-ubyte chunk (+ m_pos m_len))))
					(++ m_len))

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
						(setq rem (- rem 255)))
					(set-byte tok_buf t_pos rem) (++ t_pos))

				(defq t_mid t_pos)

				; Write Offset (16-bit LE)
				(defq offset (- i m_pos))
				(set-short tok_buf t_pos offset) (setq t_pos (+ t_pos 2))

				; Write Match Length Extras
				(when (>= (- m_len 4) 15)
					(defq rem (- m_len 19))
					(while (>= rem 255)
						(set-byte tok_buf t_pos 255) (++ t_pos)
						(setq rem (- rem 255)))
					(set-byte tok_buf t_pos rem) (++ t_pos))

				(defq t_end t_pos)

				; Splice strictly alternates between chunk and tok_buf.
				; (Empty chunk) -> (Token + Lit Extra) -> (Literals) -> (Offset + Match Extra)
				(push indices 0 0 t_start t_mid anchor i t_mid t_end)

				; Fast forward over the match
				(setq i (+ i m_len) anchor i))
			(:t
				(++ i))))

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
			(setq rem (- rem 255)))
		(set-byte tok_buf t_pos rem) (++ t_pos))

	(defq t_mid t_pos)
	; Final append: (Empty chunk) -> (Final Token + Extras) -> (Final Literals)
	(push indices 0 0 t_start t_mid anchor c_len)

	; Perform the atomic slice/concat!
	(splice chunk tok_buf indices))

(defun lz4-decode-raw (chunk window block_size)
	; (lz4-decode-raw string string num) -> string
	; Decodes a raw LZ4 block, resolving overlaps smoothly.
	(defq dec_block (str-alloc block_size)
		  c_pos 0 c_len (length chunk)
		  d_pos 0 w_len (length window))

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
				(setq lit_len (+ lit_len v))))

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
					(setq match_len (+ match_len v))))
			(setq match_len (+ match_len 4))

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
	; (lz4-compress in_stream out_stream window_size)
	(setd window_size 65536)
	(defq hash_table (cap 8192 (nums)))
	(times 8192 (push hash_table -65536))

	; Write Standard LZ4 Frame Header
	; FLG=0x60 (V1, Block Indep), BD=0x40 (64KB Max) -> HC=0x82
	(write-int out_stream +lz4_magic)
	(write-byte out_stream '(0x60 0x40 0x82))

	(defq chunk (read-blk in_stream window_size))
	(while chunk
		(task-slice)
		(times 8192 (elem-set hash_table (!) -65536)) ; Reset hash table
		
		(defq comp (lz4-compress-raw chunk hash_table))
		
		(if (>= (length comp) (length chunk))
			(progn
				; Highest bit set indicates uncompressed
				(write-int out_stream (logior 0x80000000 (length chunk)))
				(write-blk out_stream chunk))
			(progn
				; Output compressed length
				(write-int out_stream (length comp))
				(write-blk out_stream comp)))
				
		(setq chunk (read-blk in_stream window_size)))

	; Write 0-byte EndMark
	(write-int out_stream 0))

(defun lz4-decompress (in_stream out_stream &optional window_size)
	; (lz4-decompress in_stream out_stream window_size)
	(setd window_size 65536)
	(when (= (read-int in_stream) +lz4_magic)
		(defq flg (read-byte in_stream)
			  bd  (read-byte in_stream)
			  hc  (read-byte in_stream))

		; Frame defines independent blocks or sliding window
		(defq block_indep (bits? flg 0x20)
			  window "" running :t)

		(while running
			(task-slice)
			(defq size (read-uint in_stream))
			(if (or (not size) (= size 0))
				(setq running :nil) ; EndMark or EOF detected
				(progn
					(defq uncompressed (bits? size 0x80000000)
						  real_size (logand size 0x7FFFFFFF)
						  chunk (read-blk in_stream real_size))

					(if uncompressed
						(progn
							(write-blk out_stream chunk)
							(if (not block_indep) (setq window (cat window chunk))))
						(progn
							(defq dec_block (lz4-decode-raw chunk window window_size))
							(write-blk out_stream dec_block)
							(if (not block_indep) (setq window (cat window dec_block)))))

					; Using negative index to cleanly truncate window circular buffer
					(if (and (not block_indep) (> (length window) 65536))
						(setq window (slice window -65536 -1))))))))

;module
(export-symbols '(lz4-compress lz4-decompress lz4-compress-raw lz4-decode-raw))
(env-pop)