;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AES-GCM, the sealing of NIST SP 800-38D, as TLS 1.2 uses it.
;
; (gcm-key key) -> gkey, of a key of 16 or 32 bytes.
; (gcm-seal gkey nonce aad data) -> the data encrypted, with a tag of
; 16 bytes after it that says it and the aad have not been changed.
; (gcm-open gkey nonce aad sealed) -> the data, or :nil if the tag
; says it, or the aad, was changed.
;
; The nonce is 12 bytes and must never be used twice with a key. The
; data is encrypted with AES in counter mode, lib/crypto/aes.inc, from
; the block after the nonce's first, and the tag is the hash of the aad
; and what was encrypted, GHASH, times the key's hash of nothing, with
; that first block encrypted added. The work of the hash on each block
; is native code, lib/crypto/lisp.vp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./aes.inc")

(ffi (path-to-absolute "./ghash_blocks") ghash-blocks)
; (ghash-blocks state h data offset count) -> state

;module
(env-push)

;how many blocks the native code is given at a time, 64KB, between which
;the other tasks of the node have a turn
(defq +gcm_slice 4096 +gcm_zeros (apply (const cat) (map (lambda (&) (char 0)) (range 0 16))))

(defun gcm-key (key)
	; (gcm-key key) -> (keys h)
	(defq keys (aes-key key))
	(list keys (aes-block keys +gcm_zeros)))

(defun gcm-hash (state h data)
	; (gcm-hash state h data) -> state
	;the data taken into the hash, the last block made up with zeros
	(defq whole (/ (length data) 16) at 0)
	(while (< at whole)
		(defq some_blocks (min (- whole at) +gcm_slice))
		(ghash-blocks state h data (* at 16) some_blocks)
		(setq at (+ at some_blocks))
		(if (< at whole) (task-slice)))
	(if (/= (% (length data) 16) 0)
		(ghash-blocks state h (cat (slice data (* whole 16) -1) (slice +gcm_zeros 0 (- 16 (% (length data) 16)))) 0 1))
	state)

(defun gcm-tag (gkey j0 aad cipher)
	; (gcm-tag gkey j0 aad cipher) -> str
	(bind '(keys h) gkey)
	;a new str each time, the hash is made in it in place
	(defq state (cat (char 0 8) (char 0 8)))
	(gcm-hash state h aad)
	(gcm-hash state h cipher)
	(ghash-blocks state h (cat (char-be (* (length aad) 8) 8) (char-be (* (length cipher) 8) 8)) 0 1)
	(aes-ctr keys j0 state))

(defun gcm-seal (gkey nonce aad data)
	; (gcm-seal gkey nonce aad data) -> str
	(defq j0 (cat nonce (char-be 1 4)) cipher (aes-ctr (first gkey) (aes-count j0 1) data))
	(cat cipher (gcm-tag gkey j0 aad cipher)))

(defun gcm-open (gkey nonce aad sealed)
	; (gcm-open gkey nonce aad sealed) -> :nil | str
	(when (>= (length sealed) 16)
		(defq j0 (cat nonce (char-be 1 4)) cipher (slice sealed 0 -17))
		(if (eql (gcm-tag gkey j0 aad cipher) (slice sealed -17 -1))
			(aes-ctr (first gkey) (aes-count j0 1) cipher))))

;module
(export-symbols '(gcm-key gcm-seal gcm-open))
(env-pop)
