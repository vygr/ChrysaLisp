;docs/ai_digest/link_protocol.md: the door of a TCP link, what each end sends and checks, in order
(defq s (dia-scene) blue 0xffd7e6f7 green 0xffdff0d8 gold 0xffffe8a8 grey 0xfff2f4f6
	mid 330.0 left 80 right 580)
(dia-put s :nil (dia-box '("dialler" "lower system id, or told") 160 44 blue 11 :first) 0 0)
(dia-put s :nil (dia-box '("listener" "port 3333") 160 44 green 11 :first) 500 0)
;the two ends, down the page
(each (# (push (first s) (cwb-shape (cwb-d "M" %0 44 "L" %0 352) :stroke 0xffb0b8c0 :width 1.2)))
	(list left right))
(defun door-centre (text y &optional size bold)
	(dia-say s text (- mid (* 0.5 (dia-width text (ifn size 11) bold))) y (ifn size 11) bold
		(if bold 0xff202428 0xff5c6670)))
(defun door-both (y title under)
	;a message each end sends the other, the same of each
	(push (first s) (dia-arrow (n2f left) (n2f y) (n2f right) (n2f y) :both))
	(door-centre title (- (n2f y) 6.0) 12 :t)
	(door-centre under (+ (n2f y) 16.0)))
(defun door-each (y text fill)
	;what each end does by itself, beside it
	(dia-put s :nil (dia-box text 150 34 fill 11) (- left 75) (- y 17))
	(dia-put s :nil (dia-box text 150 34 fill 11) (- right 75) (- y 17)))
(door-both 80 "1. hello, 60 bytes" "CLDOOR3, the version, a nonce, an X25519 share, 1 if the dialler seals")
(door-each 134 '("2. first 11 bytes its own?" "no: close, at once") gold)
(door-centre "3. what was said, 97 bytes: both nonces, both shares, the seal byte" 140)
(door-both 190 "4. proof, 32 bytes" "HMAC-SHA256(mesh key, hello's 11 + D or L + what was said)")
(door-each 244 '("5. the other's proof right?" "no: close") gold)
(door-centre "each end knows the other has the key" 250)
(door-each 296 '("6. sealed: X25519, HKDF" "k_dl and k_ld") grey)
(door-centre "not sealed: no keys" 302)
(door-both 340 "pings, then mail" "")
(dia-say s "All of it in 5 seconds; an end that finds anything wrong closes the socket and says nothing." 0 378 11)
(diagram "link_door" (dia-scene-doc s))
