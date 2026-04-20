;;;;;;;;;;;
; ring buffer
;;;;;;;;;;;

;module
(env-push)

(defclass Ring (capacity_req) :nil
	; (Ring capacity_req) -> ring
	; Creates a generic ring-buffer over a :str
	(defq _cap 1)
	(while (< _cap capacity_req) (setq _cap (<< _cap 1)))
	(def this :capacity _cap :mask (- _cap 1) :pos 0 :buf (str-alloc _cap))

	(defgetmethod :pos)

	(defmethod :write (data)
		; (. ring :write data) -> ring
		(raise :capacity :mask :buf :pos)
		(defq slen (length data) start (logand pos mask))
		; Safe-guard: if payload is larger than ring, only keep the tail
		(when (> slen capacity)
			(setq data (slice data (- slen capacity) slen)
				slen capacity))

		(defq space (- capacity start))
		(if (<= slen space)
			; Fits without wrapping
			(set-str buf start data)
			; Wraps around buffer physically
			(progn
				(set-str buf start (slice data 0 space))
				(set-str buf 0 (slice data space slen))))
		(setq pos (+ pos slen))
		(lower :pos)
		this)

	(defmethod :match (offset match_len)
		; (. ring :match offset match_len) -> ring
		(raise :capacity :mask :buf :pos)
		(if (>= offset match_len)
			; No physical overlap in logical stream, can bulk copy
			(progn
				(defq m_pos (logand (- pos offset) mask)
					space (- capacity m_pos))
				(if (<= match_len space)
					; Source doesn't wrap
					(. this :write (slice buf m_pos (+ m_pos match_len)))
					; Source wraps around boundary
					(progn
						(. this :write (slice buf m_pos capacity))
						(. this :write (slice buf 0 (- match_len space))))))
			; Overlap - fallback to byte-by-byte for RLE replication mechanism
			(progn
				(defq m_pos (- pos offset) b buf m mask p pos)
				(times match_len
					(set-byte b (logand p m) (get-ubyte b (logand m_pos m)))
					(++ m_pos) (++ p))
				(setq pos (+ pos match_len))
				(lower :pos)))
		this)

	(defmethod :write_stream (stream out_len)
		; (. ring :write_stream stream out_len) -> ring
		(raise :capacity :mask :buf :pos)
		(defq start (logand (- pos out_len) mask)
			space (- capacity start))
		(if (<= out_len space)
			(write-blk stream (slice buf start (+ start out_len)))
			(progn
				(write-blk stream (slice buf start capacity))
				(write-blk stream (slice buf 0 (- out_len space)))))
		this)
	)

;module
(export-classes '(Ring))
(env-pop)
