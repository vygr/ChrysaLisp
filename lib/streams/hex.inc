;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Hex stream encoding & decoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(bits +hex_stream_flag 0
	(bit offset chars))

(defq +hex_flag_offset +hex_stream_flag_offset
	+hex_flag_chars +hex_stream_flag_chars)

(defun hex-encode-stream (in_stream out_stream &optional chunk_size flags)
	; (hex-encode-stream in_stream out_stream [chunk_size flags])
	(setd chunk_size 8 flags (+ +hex_stream_flag_offset +hex_stream_flag_chars))
	(defq adr 0)
	(while (defq blk (read-blk in_stream chunk_size))
		(task-slice)
		(defq parts (list))
		(if (bits? flags +hex_stream_flag_offset)
			(push parts (cat (int-to-hex-str adr) " ")))
		(if (bits? flags +hex_stream_flag_chars)
			(push parts
				(join (partition (hex-encode blk) 2) " " 2)
				(pad "" (* 3 (- chunk_size (length blk))) " ")
				(apply (const cat)
					(map (# (if (bfind %0 +char_class_printable) %0 ".")) blk)))
			(push parts (join (partition (hex-encode blk) 2) " ")))
		(write-line out_stream (apply (const cat) parts))
		(setq adr (+ adr chunk_size)))
	(stream-flush out_stream))

(defun hex-decode-stream (in_stream out_stream &optional chunk_size flags)
	; (hex-decode-stream in_stream out_stream [chunk_size flags])
	(setd chunk_size 8 flags (+ +hex_stream_flag_offset +hex_stream_flag_chars))
	(defq hex_offset (if (bits? flags +hex_stream_flag_offset) 9 0))
	(while (defq line (read-line in_stream))
		(task-slice)
		(setq line (trim-end line "\r"))
		(when (>= (length line) hex_offset)
			(defq hex_part (if (bits? flags +hex_stream_flag_chars)
				(slice line hex_offset (min (length line) (+ hex_offset (* 3 chunk_size))))
				(slice line hex_offset -1)))
			(defq tokens (split hex_part " "))
			(when (nempty? tokens)
				(write-blk out_stream (hex-decode (apply (const cat) tokens))))))
	(stream-flush out_stream))

(defq hex-compress hex-encode-stream
	hex-decompress hex-decode-stream)

;module
(export-symbols '(+hex_stream_flag_offset +hex_stream_flag_chars
	+hex_flag_offset +hex_flag_chars
	hex-encode-stream hex-decode-stream
	hex-compress hex-decompress))
(env-pop)
