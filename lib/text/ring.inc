;;;;;;;;;;;
; ring buffer
;;;;;;;;;;;

;module
(env-push)

(ffi (path-to-absolute "./ring_write") ring-write)
(ffi (path-to-absolute "./ring_match") ring-match)
(ffi (path-to-absolute "./ring_write_stream") ring-write-stream)

(defclass Ring (capacity_req) :nil
	; (Ring capacity_req) -> ring
	; Creates a generic ring-buffer over a :str
	(defq _cap 1)
	(while (< _cap capacity_req) (setq _cap (<< _cap 1)))
	(def this :capacity _cap :mask (dec _cap) :pos 0 :buf (str-alloc _cap))

	(defgetmethod :pos)
	(defsetmethod :pos)
	(defgetmethod :buf)

	(defmethod :write (data)
		; (. ring :write data) -> ring
		(raise :buf :pos)
		(lower (:pos (ring-write buf pos data)))
		this)

	(defmethod :match (offset match_len)
		; (. ring :match offset match_len) -> ring
		(raise :buf :pos)
		(lower (:pos (ring-match buf pos offset match_len)))
		this)

	(defmethod :write_stream (stream out_len)
		; (. ring :write_stream stream out_len) -> ring
		(raise :buf :pos)
		(ring-write-stream buf pos stream out_len)
		this)
	)

;module
(export-classes '(Ring))
(env-pop)
