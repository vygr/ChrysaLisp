;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HKDF with SHA-256, RFC 5869: keys made from a secret.
;
; (hkdf-extract salt secret) -> 32 bytes, the secret made even.
; (hkdf-expand prk info length) -> length bytes of it, for info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./sha256.inc")

(defun hkdf-extract (salt secret)
	; (hkdf-extract salt secret) -> str
	(hmac-sha256 (if (empty? salt) (apply (const cat) (map (lambda (&) (char 0)) (range 0 32))) salt) secret))

(defun hkdf-expand (prk info length)
	; (hkdf-expand prk info length) -> str
	(defq out "" block "" n 0)
	(while (< (length out) length)
		(setq n (inc n) block (hmac-sha256 prk (cat block info (char n))) out (cat out block)))
	(slice out 0 length))
