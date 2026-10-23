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
(import "./same.inc")

(ffi (path-to-absolute "./aead_seal") aead-seal-native)
; (aead-seal-native key nonce data) -> str
(ffi (path-to-absolute "./aead_open") aead-open-native)
; (aead-open-native key nonce sealed) -> :nil | str
;with no aad, all of it native code, :sys_crypto, as a link seals

(defq +aead_zeros (apply (const cat) (map (lambda (&) (char 0 8)) (range 0 8))))

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
	(cond
		((eql aad "") (aead-seal-native key nonce data))
		(:t (defq cipher (chacha20 key nonce 1 data))
			(cat cipher (aead-tag key nonce aad cipher)))))

(defun aead-open-lisp (key nonce aad sealed)
	; (aead-open-lisp key nonce aad sealed) -> :nil | str
	(when (>= (defq size (- (length sealed) 16)) 0)
		(defq cipher (slice sealed 0 size))
		(if (crypto-same? (aead-tag key nonce aad cipher) (slice sealed size -1))
			(chacha20 key nonce 1 cipher))))

(defun aead-open (key nonce aad sealed)
	; (aead-open key nonce aad sealed) -> :nil | str
	(cond
		((eql aad "") (aead-open-native key nonce sealed))
		((aead-open-lisp key nonce aad sealed))))
