;;;;;;;;;;;;;;;;;;;;;;
; LZ4 Fast Compression
;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(ffi (path-to-absolute "./lz4_encode_raw") lz4-encode-raw)
(ffi (path-to-absolute "./lz4_decode_raw") lz4-decode-raw)
(ffi (path-to-absolute "./lz4_write_stream") lz4-write-stream)
(ffi (path-to-absolute "./lz4_write") lz4-write)

(defq +lz4_magic 0x184D2204)

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
				(setq pos (lz4-decode-raw ring_buf pos chunk)))
			; output decoded bytes
			(lz4-write-stream ring_buf pos out_stream (- pos old_pos)))))

;module
(export-symbols '(lz4-compress lz4-decompress lz4-compress-raw lz4-decode-raw))
(env-pop)