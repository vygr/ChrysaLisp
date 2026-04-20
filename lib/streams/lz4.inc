;;;;;;;;;;;;;;;;;;;;;;
; LZ4 Fast Compression
;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +lz4_magic 0x184D2204)

; 13 bits = 8192 slots
(defmacro lz4-hash (v) `(logand (hash ,v) 8191))

(defun lz4-write-len (buf pos len)
	; (lz4-write-len buf pos len) -> pos
	; Write LZ4 variable length extras to buf at pos.
	(while (>= len 255)
		(set-byte buf pos 255) (++ pos)
		(-- len 255))
	(set-byte buf pos len) (++ pos))

(defun lz4-read-len (chunk c_pos base)
	; (lz4-read-len chunk c_pos base) -> (c_pos base)
	; Read LZ4 variable length extras from chunk, returns updated pos and length.
	(defq v 255)
	(while (= v 255)
		(setq v (get-ubyte chunk c_pos))
		(++ c_pos) (++ base v))
	(list c_pos base))

(defun lz4-compress-raw (window start_idx hash_table)
	; (lz4-compress-raw window start_idx hash_table) -> string
	; Compresses the new chunk appended to the sliding window.
	(defq i start_idx anchor start_idx c_len (length window)
		tok_buf (str-alloc (+ (- c_len start_idx) 65536))
		t_pos 0 indices (nums))

	(while (< i (- c_len 12))
		(task-slice)
		(defq val (get-uint window i) h (lz4-hash val)
			m_pos (elem-get hash_table h))
		(elem-set hash_table h i)
		(cond
			((and (>= m_pos 0)
					(<= (- i m_pos) 65535)
					(= val (get-uint window m_pos)))
				; match found, extend match length
				(defq m_len 4)
				(some! (# (if (eql %0 (elem-get window (+ m_pos m_len))) (++ m_len)))
					(list window) :t (+ i m_len) (- c_len 5))
				; token
				(defq lit_len (- i anchor)
					token (logior (<< (min lit_len 15) 4) (min (- m_len 4) 15))
					t_start t_pos)
				(set-byte tok_buf t_pos token) (++ t_pos)
				; literal length extras
				(when (>= lit_len 15)
					(setq t_pos (lz4-write-len tok_buf t_pos (- lit_len 15))))
				; offset (16-bit LE)
				(defq t_mid t_pos)
				(set-short tok_buf t_pos (- i m_pos)) (++ t_pos 2)
				; match length extras
				(when (>= (- m_len 4) 15)
					(setq t_pos (lz4-write-len tok_buf t_pos (- m_len 19))))
				; build splice: (empty) (token+lit extras) (literals) (offset+match extras)
				(push indices 0 0 t_start t_mid anchor i t_mid t_pos)
				; advance past match
				(setq i (+ i m_len) anchor i))
			((++ i))))

	; final literal sequence
	(defq lit_len (- c_len anchor) t_start t_pos)
	(set-byte tok_buf t_pos (<< (min lit_len 15) 4)) (++ t_pos)
	(when (>= lit_len 15)
		(setq t_pos (lz4-write-len tok_buf t_pos (- lit_len 15))))
	(push indices 0 0 t_start t_pos anchor c_len)
	(splice window tok_buf indices))

(defun lz4-decode-raw (chunk dec_buf d_pos)
	; (lz4-decode-raw chunk dec_buf d_pos) -> d_pos
	; Decodes a raw LZ4 block into dec_buf at d_pos.
	; dec_buf[0..d_pos) contains the sliding window history.
	(defq c_pos 0 c_len (length chunk))

	(while (< c_pos c_len)
		(task-slice)
		(defq token (get-ubyte chunk c_pos)) (++ c_pos)

		; literals
		(defq lit_len (>> token 4))
		(when (= lit_len 15)
			(bind '(c_pos lit_len) (lz4-read-len chunk c_pos lit_len)))
		(when (> lit_len 0)
			(set-str dec_buf d_pos (slice chunk c_pos (++ c_pos lit_len)))
			(++ d_pos lit_len))

		; match (if not at EOF)
		(when (< c_pos c_len)
			(defq offset (logior (get-ubyte chunk c_pos)
				(<< (get-ubyte chunk (++ c_pos)) 8)))
			(++ c_pos)
			(defq ml (logand token 0x0F) match_len (+ ml 4))
			(when (= ml 15)
				(bind '(c_pos match_len) (lz4-read-len chunk c_pos match_len)))
			(defq m_pos (- d_pos offset))
			(if (>= offset match_len)
				; no overlap: fast block copy
				(set-str dec_buf d_pos (slice dec_buf m_pos (+ m_pos match_len)))
				; overlap: byte-by-byte for RLE repeating patterns
				(for d_pos (+ d_pos match_len)
					(set-byte dec_buf (!) (get-ubyte dec_buf m_pos))
					(++ m_pos)))
			(++ d_pos match_len)))
	d_pos)

(defun lz4-compress (in_stream out_stream &optional window_size)
	; (lz4-compress in_stream out_stream [window_size])
	(setd window_size 65536)
	(defq hash_table (cap 8192 (list)) window "")
	(times 8192 (push hash_table -65536))
	;standard LZ4 Frame Header
	;FLG=0x40 (V1, Block Dep), BD=0x40 (64KB Max) -> HC=0x73
	(write-int out_stream +lz4_magic)
	(write-byte out_stream '(0x40 0x40 0x73))
	(while (defq chunk (read-blk in_stream window_size))
		(task-slice)
		;append and truncate sliding window
		(defq chunk_len (length chunk) win_len (length window))
		(cond
			((= win_len 0)
				;first chunk
				(defq start_idx 0 window chunk))
			(:t	;subsequent chunks: keep exactly the previous chunk as history
				(defq shift (- win_len window_size) start_idx window_size
					window (splice window chunk (nums (- -1 window_size) -1 0 -1)))
				;shift hash table down if the window actually slid (Iteration 3+)
				(if (> shift 0) (setq hash_table (map (# (- %0 shift)) hash_table)))))
		;compress the newly appended chunk portion
		(defq comp (lz4-compress-raw window start_idx hash_table))
		(cond
			((>= (length comp) chunk_len)
				;highest bit set indicates uncompressed
				(write-int out_stream (logior 0x80000000 chunk_len))
				(write-blk out_stream chunk))
			(:t ;output compressed length
				(write-int out_stream (length comp))
				(write-blk out_stream comp))))
	;write 0 EndMark
	(write-int out_stream 0))

(defun lz4-decompress (in_stream out_stream &optional window_size)
	; (lz4-decompress in_stream out_stream [window_size])
	(setd window_size 65536)
	(when (and (defq magic (read-int in_stream)) (= magic +lz4_magic))
		(defq flg (read-byte in_stream) bd (read-byte in_stream) hc (read-byte in_stream)
			max_block_size (<< 1 (+ 8 (* 2 (logand (>> bd 4) 7))))
			block_indep (bits? flg 0x20)
			decode_space (str-alloc max_block_size)
			dec_buf "" d_pos 0)
		(while (and (defq size (read-uint in_stream)) (/= size 0))
			(task-slice)
			(defq chunk (read-blk in_stream (logand size 0x7FFFFFFF))
				w_start (if block_indep d_pos (max 0 (- d_pos window_size)))
				w_len (- d_pos w_start))
			; single splice: window tail from old buffer + new data source
			(if (bits? size 0x80000000)
				; uncompressed: window tail + chunk
				(setq dec_buf (splice dec_buf chunk (nums w_start d_pos 0 -1))
					d_pos (+ w_len (length chunk)))
				; compressed: window tail + decode space, then decode
				(setq dec_buf (splice dec_buf decode_space (nums w_start d_pos 0 -1))
					d_pos (lz4-decode-raw chunk dec_buf w_len)))
			; output decoded bytes
			(write-blk out_stream (slice dec_buf w_len d_pos)))))

;module
(export-symbols '(lz4-compress lz4-decompress lz4-compress-raw lz4-decode-raw))
(env-pop)