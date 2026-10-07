;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChaCha20, the cipher of RFC 8439.
;
; (chacha20 key nonce counter data) -> the data, each byte xored
; with the next of a stream of bytes that the key, the nonce and
; the counter make. Done again with the same three it gives the
; data back, so it is how to encrypt and how to decrypt.
;
; The key is a str of 32 bytes, the nonce one of 12. A nonce must
; never be used twice with a key. The counter is the number of the
; first block of 64 bytes of the stream, 0, or 1 where block 0 is
; kept for something else.
;
; It hides data and does not guard it, a byte changed on the way
; is a byte changed when it is decrypted, and nothing says so.
;
; The work is native code, lib/crypto/lisp.vp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi (path-to-absolute "./chacha20_xor") chacha20-xor)
; (chacha20-xor key nonce counter data out offset length) -> out

;how many bytes the native code is given at a time, a whole number of
;blocks, between which the other tasks of the node have a turn
(defq +chacha20_slice 65536)

(defun chacha20 (key nonce counter data)
	; (chacha20 key nonce counter data) -> str
	(defq size (length data) out (str-alloc size) at 0)
	(while (< at size)
		(defq some_bytes (min (- size at) +chacha20_slice))
		(chacha20-xor key nonce (+ counter (/ at 64)) data out at some_bytes)
		(setq at (+ at some_bytes))
		(if (< at size) (task-slice)))
	out)
