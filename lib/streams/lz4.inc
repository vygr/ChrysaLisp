;;;;;;;;;;;;;;;;;;;;;;
; LZ4 Fast Compression
;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(ffi (path-to-absolute "./lz4_encode_raw") lz4-encode-raw)
(ffi (path-to-absolute "./lz4_decode_raw") lz4-decode-raw)
(ffi (path-to-absolute "./lz4_write") lz4-write)
(ffi (path-to-absolute "./lz4_write_stream") lz4-write-stream)

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

(defun lz4-encode-raw-lisp (window start_idx hash_table)
	; (lz4-encode-raw window start_idx hash_table) -> string
	; Compresses the new chunk appended to the sliding window.
	(defq i start_idx anchor start_idx c_len (length window)
		tok_buf (str-alloc (+ (- c_len start_idx) 65536))
		t_pos 0 indices (nums))
	(while (< i (- c_len 12))
		(task-slice)
		(defq val (get-uint window i) h (lz4-hash val) m_pos (elem-get hash_table h))
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

(defun lz4-compress (in_stream out_stream &optional window_size)
	; (lz4-compress in_stream out_stream [window_size])
	(setd window_size 65536)
	(defq hash_table (cap 8192 (nums)) window "")
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
				(defq start_idx 0 window chunk shift 0))
			(:t	;subsequent chunks: keep exactly the previous chunk as history
				(defq shift (- win_len window_size) start_idx window_size
					window (splice window chunk (nums (- -1 window_size) -1 0 -1)))))
		;compress the newly appended chunk portion
		(defq comp (lz4-encode-raw window start_idx hash_table shift))
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
			ring_size (max window_size max_block_size)
			window_size 1 pos 0)
		(while (< window_size ring_size) (setq window_size (<< window_size 1)))
		(defq ring_buf (str-alloc window_size))
		(while (and (defq size (read-uint in_stream)) (/= size 0))
			(task-slice)
			(defq chunk (read-blk in_stream (logand size 0x7FFFFFFF)) old_pos pos)
			(if (bits? size 0x80000000)
				; uncompressed
				(setq pos (lz4-write ring_buf pos chunk))
				; compressed
				(setq pos (lz4-decode-raw chunk ring_buf pos)))
			; output decoded bytes
			(lz4-write-stream ring_buf pos out_stream (- pos old_pos)))))

;module
(export-symbols '(lz4-compress lz4-decompress lz4-compress-raw lz4-decode-raw))
(env-pop)