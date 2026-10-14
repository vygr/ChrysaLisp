;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Base64, RFC 4648: bytes as letters, digits, + and /, 3 bytes to 4
;
; (base64-encode data) -> str
; (base64-decode text) -> :nil | str
;
; What a PEM file has between its BEGIN and END lines. The decode passes
; over what is not of the alphabet, the ends of lines say, and stops at =.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +base64_chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64-encode (data)
	; (base64-encode data) -> str
	(defq out (string-stream (cat "")) at 0 len (length data))
	(while (< at len)
		(defq n (min 3 (- len at))
			v (logior (<< (code data 1 at) 16)
				(if (> n 1) (<< (code data 1 (inc at)) 8) 0)
				(if (> n 2) (code data 1 (+ at 2)) 0)))
		(write-blk out (cat (elem-get +base64_chars (>> v 18)) (elem-get +base64_chars (logand (>> v 12) 63))
			(if (> n 1) (elem-get +base64_chars (logand (>> v 6) 63)) "=")
			(if (> n 2) (elem-get +base64_chars (logand v 63)) "=")))
		(setq at (+ at 3)))
	(str out))

(defun base64-decode (text)
	; (base64-decode text) -> :nil | str
	;:nil if what is left at the end is not whole bytes
	(defq out (string-stream (cat "")) acc 0 have 0 done :nil)
	(each (lambda (c)
			(cond
				(done)
				((eql c "=") (setq done :t))
				((defq v (find c +base64_chars))
					(setq acc (logior (<< (logand acc 0xffff) 6) v) have (+ have 6))
					(when (>= have 8)
						(setq have (- have 8))
						(write-char out (logand (>> acc have) 0xff))))))
		text)
	(if (< have 6) (str out)))

;module
(export-symbols '(base64-encode base64-decode))
(env-pop)
