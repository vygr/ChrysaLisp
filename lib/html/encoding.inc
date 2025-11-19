;; HTML Character Encoding Detection and Conversion
;; Simplified version of KEncodingDetector

(import "lib/class/class.inc")

(defclass encoding-detector (&optional _method) :nil
	; Character encoding detector

	; Initialize fields using correct ChrysaLisp syntax
	(def this
		:encoding_name "utf-8"
		:encoding_source :default
		:has_invalid_chars :nil
		:buffer (list))

	(defmethod :init ()
		; Initialize encoding detector
		(def this
			:encoding_name "utf-8"
			:encoding_source :default
			:has_invalid_chars :nil
			:buffer (list))
		this)

	(defmethod :set-encoding (enc source)
		; Set the character encoding
		; enc: encoding name (e.g. "utf-8", "iso-8859-1")
		; source: encoding source (:user, :http-header, :default, :auto)
		(def this :encoding_name (to-lower enc))
		(def this :encoding_source source)
		:t)

	(defmethod :get-encoding ()
		; Get current encoding name
		(get :encoding_name this))

	(defmethod :reset ()
		; Reset the decoder state
		(def this :has_invalid_chars :nil)
		(def this :buffer (list))
		this)

	(defmethod :decoded-invalid-chars? ()
		; Check if invalid characters were encountered during decoding
		(get :has_invalid_chars this))

	(defmethod :decode (data)
		; Decode byte data to string
		; data: string of bytes to decode
		; Returns decoded string
		(defq result (cat data))

		; Check for invalid UTF-8 if encoding is UTF-8
		(when (eql (get :encoding_name this) "utf-8")
			(defq prev :nil)
			(each! (lambda (ch)
				(defq byte (code ch))
				(unless (is-valid-utf8-byte? byte prev)
					(def this :has_invalid_chars :t))
				(setq prev byte))
				(split data "") 0 -1))

		result)

	(defmethod :decode-with-buffering (data)
		; Decode with buffering - accumulate data until encoding is determined
		; For now, just buffer if encoding source is :default or :auto
		(defq enc-source (get :encoding_source this))
		(if (or (eql enc-source :default) (eql enc-source :auto))
			(progn
				(defq buf (get :buffer this))
				(push buf data)
				(def this :buffer buf)
				"")  ; Return empty string while buffering
			(. this :decode data)))

	(defmethod :flush ()
		; Flush buffered data and decode it
		(defq buf (get :buffer this))
		(if (empty? buf)
			""
			(progn
				(defq all-data (apply cat (reverse buf)))
				(def this :buffer (list))
				(. this :decode all-data)))))

;; Helper functions (not methods)

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

