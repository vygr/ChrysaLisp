(include "lib/asm/class.inc")

;;;;;;;;;;;;;;;;;;;;;
; crypto static class
;;;;;;;;;;;;;;;;;;;;;

;ChaCha20 and Poly1305 of RFC 8439, on memory, for native code: what seals
;a link over the internet, service/net/link.vp, and what lib/crypto's Lisp
;natives call. A Poly1305 state is +poly1305_state_size bytes, h then r
;then 5 times the top four of r, then the slot of the top bit

(defcvar '+poly1305_state_size 120 '+aead_tag_size 16)

(def-class :sys_crypto :nil
	(dec-method :chacha20 sys/crypto/chacha20 :static (:r0 :r1 :r2 :r3 :r4 :r5))
	(dec-method :poly1305_init sys/crypto/poly1305_init :static (:r0 :r1))
	(dec-method :poly1305_blocks sys/crypto/poly1305_blocks :static (:r0 :r1 :r2 :r3))
	(dec-method :poly1305_finish sys/crypto/poly1305_finish :static (:r0 :r1 :r2))
	(dec-method :aead_seal sys/crypto/aead_seal :static (:r0 :r1 :r2 :r3 :r4))
	(dec-method :aead_open sys/crypto/aead_open :static (:r0 :r1 :r2 :r3 :r4) (:r0)))
