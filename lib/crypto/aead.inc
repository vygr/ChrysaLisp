;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChaCha20 with Poly1305, the AEAD of RFC 8439. Data that is hidden
; and guarded both, which is what a cipher is wanted for.
;
; (aead-seal key nonce aad data) -> the data encrypted, with a tag
; of 16 bytes after it.
;
; (aead-open key nonce aad sealed) -> the data, or :nil if the tag
; is not right, when a byte of it, of the aad, of the key or of the
; nonce is not what it was sealed with.
;
; The key is a str of 32 bytes, the nonce one of 12. A nonce must
; never be used twice with a key. aad is data that goes with it and
; is guarded but not hidden, the name of a thing, a header, or "".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./chacha20.inc")
(import "./poly1305.inc")

(defq +aead_zeros (apply (const cat) (map (lambda (_) (char 0 8)) (range 0 8))))

(defun aead-tag (key nonce aad cipher)
	;the tag of what was encrypted and what goes with it. The key of the
	;tag is the start of block 0 of the stream, and is for this nonce
	;alone. Each of the two is made up with 0 to a whole 16 bytes, and
	;how long they were comes last
	(defq ctx (poly1305-start (slice (chacha20 key nonce 0 +aead_zeros) 0 32)))
	(each (# (when (nempty? %0)
			(poly1305-add ctx %0)
			(poly1305-add ctx (slice +aead_zeros 0 (% (- 16 (% (length %0) 16)) 16)))))
		(list aad cipher))
	(poly1305-end (poly1305-add ctx (cat (char (length aad) 8) (char (length cipher) 8)))))

(defun aead-seal (key nonce aad data)
	; (aead-seal key nonce aad data) -> str
	(defq cipher (chacha20 key nonce 1 data))
	(cat cipher (aead-tag key nonce aad cipher)))

(defun aead-open (key nonce aad sealed)
	; (aead-open key nonce aad sealed) -> :nil | str
	(when (>= (defq size (- (length sealed) 16)) 0)
		(defq cipher (slice sealed 0 size) tag (aead-tag key nonce aad cipher) differ 0)
		;every byte of the tag is looked at, right or wrong
		(each (# (setq differ (logior differ (logxor (code %0) (code %1)))))
			tag (slice sealed size -1))
		(if (= differ 0) (chacha20 key nonce 1 cipher))))
