;; HTML Character Encoding Detection and Conversion
;; Simplified version of KEncodingDetector

(defclass encoding-detector nil
	; Character encoding detector
	(defq
		encoding_name "utf-8"
		encoding_source :default
		has_invalid_chars :nil
		buffer (list)))

(defmethod :init encoding-detector ()
	; Initialize encoding detector
	(defq this-encoding_name "utf-8"
		this-encoding_source :default
		this-has_invalid_chars :nil
		this-buffer (list))
	this)

(defmethod :set-encoding encoding-detector (enc source)
	; Set the character encoding
	; enc: encoding name (e.g. "utf-8", "iso-8859-1")
	; source: encoding source (:user, :http-header, :default, :auto)
	(setq this-encoding_name (lower-case enc))
	(setq this-encoding_source source)
	:t)

(defmethod :get-encoding encoding-detector ()
	; Get current encoding name
	this-encoding_name)

(defmethod :reset encoding-detector ()
	; Reset the decoder state
	(setq this-has_invalid_chars :nil)
	(setq this-buffer (list))
	this)

(defmethod :decoded-invalid-chars? encoding-detector ()
	; Check if invalid characters were encountered during decoding
	this-has_invalid_chars)

(defun is-valid-utf8-byte? (byte prev-byte)
	; Check if byte is valid UTF-8
	; Simplified check - just look for invalid continuation bytes
	(cond
		((< byte 128) :t)  ; ASCII range
		((and (>= byte 128) (< byte 192))
			; Continuation byte - must follow lead byte
			(and prev-byte (>= prev-byte 192)))
		((and (>= byte 192) (< byte 248)) :t)  ; Valid lead byte
		(:t :nil)))  ; Invalid byte

(defmethod :decode encoding-detector (data)
	; Decode byte data to string
	; data: string of bytes to decode
	; Returns decoded string
	(defq result (cat data))

	; Check for invalid UTF-8 if encoding is UTF-8
	(when (= this-encoding_name "utf-8")
		(defq prev nil)
		(each! 0 -1 (lambda (ch)
			(defq byte (ascii-code ch))
			(unless (is-valid-utf8-byte? byte prev)
				(setq this-has_invalid_chars :t))
			(setq prev byte))
			(split data "")))

	result)

(defmethod :decode-with-buffering encoding-detector (data)
	; Decode with buffering - accumulate data until encoding is determined
	; For now, just buffer if encoding source is :default or :auto
	(if (or (= this-encoding_source :default) (= this-encoding_source :auto))
		(progn
			(push this-buffer data)
			"")  ; Return empty string while buffering
		(. this :decode data)))

(defmethod :flush encoding-detector ()
	; Flush buffered data and decode it
	(if (empty? this-buffer)
		""
		(defq all-data (apply cat (reverse this-buffer)))
		(setq this-buffer (list))
		(. this :decode all-data))))
