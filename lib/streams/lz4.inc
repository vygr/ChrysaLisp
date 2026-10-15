;;;;;;;;;;;;;;;;;;;;;;
; LZ4 Fast Compression
;;;;;;;;;;;;;;;;;;;;;;

(import "lib/collections/fill.inc")

;module
(env-push)

(ffi (path-to-absolute "./lz4_encode") lz4-encode)
(ffi (path-to-absolute "./lz4_decode") lz4-decode)
(ffi (path-to-absolute "./lz4_write") lz4-write)
(ffi (path-to-absolute "./lz4_read") lz4-read)

;the frame: its magic, a byte of flags, the version 01 and blocks that may
;look back into the one before, a byte of the most a block holds, BD,
;64KB 256KB 1MB or 4MB, and the second byte of an xxHash32 of those two,
;each of the four worked out here as only these are written
(defq +lz4_magic 0x184D2204 +lz4_flags 0x40
	+lz4_sizes (nums 65536 262144 1048576 4194304)
	+lz4_bd (nums 0x40 0x50 0x60 0x70)
	+lz4_hc (nums 0xc0 0x77 0x96 0xdf))

(defun lz4-compress (in_stream out_stream &optional window_size)
	; (lz4-compress in_stream out_stream [window_size])
	;window_size is the most of a block, it is made one of the four a
	;frame can say, the least that holds it
	(setd window_size 65536)
	(defq i (ifn (some (# (if (<= window_size %0) (!))) +lz4_sizes) 3)
		hash_table (fill (nums) 8192 -65536) ring_buf (str-alloc (* (setq window_size (elem-get +lz4_sizes i)) 2)) pos 0)
	(write-int out_stream +lz4_magic)
	(write-byte out_stream (list +lz4_flags (elem-get +lz4_bd i) (elem-get +lz4_hc i)))
	(while (defq chunk (read-blk in_stream window_size))
		(task-slice)
		(defq chunk_len (length chunk))
		;compress the newly appended chunk portion
		(bind '(comp pos) (lz4-encode ring_buf pos chunk hash_table))
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
	; (lz4-decompress in_stream out_stream [window_size]) -> :t | :nil
	;:nil if it is not a frame of version 01, or a block is more than the
	;frame said a block could be, what was got by then is written. A
	;block's sum, the content's size and a dictionary's id are passed
	;over, not looked at
	(setd window_size 65536)
	(defq ok :nil)
	(when (and (defq magic (read-int in_stream)) (= magic +lz4_magic)
			(defq flg (read-byte in_stream) bd (read-byte in_stream))
			(= (logand flg 0xc0) 0x40))
		(defq max_block_size (<< 1 (+ 8 (* 2 (logand (>> bd 4) 7))))
			block_sums (bits? flg 0x10) w 1 pos 0)
		(if (bits? flg 0x08) (read-blk in_stream 8))
		(if (bits? flg 0x01) (read-blk in_stream 4))
		(read-byte in_stream)
		(while (< w (max window_size max_block_size)) (setq w (<< w 1)))
		(defq ring_buf (str-alloc w) ok :t)
		(while (and ok (defq size (read-uint in_stream)) (/= size 0))
			(task-slice)
			(defq len (logand size 0x7FFFFFFF))
			(cond
				((> len max_block_size) (setq ok :nil))
				(:t (defq old_pos pos pos (if (bits? size 0x80000000)
						;uncompressed, read directly into ring buffer
						(lz4-read ring_buf pos in_stream len)
						;compressed
						(lz4-decode ring_buf pos (read-blk in_stream len))))
					;output decoded bytes
					(lz4-write ring_buf pos out_stream (- pos old_pos))
					(if block_sums (read-blk in_stream 4))))))
	ok)

;module
(export-symbols '(lz4-compress lz4-decompress lz4-encode lz4-decode))
(env-pop)
