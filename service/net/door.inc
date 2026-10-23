;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The door of a TCP link, for machines that share a key.
;
; A link, once made, is trusted with everything. The node at the
; other end can mail any service and start a task on any node. So
; the key guards the link itself. When two machines connect, each
; proves to the other that it holds the key, before a byte of mail
; goes over, and a connection that does not is closed.
;
; The key is the file mesh_key at the root of the tree, 64 hex
; digits, 32 bytes, or any other text, a passphrase, which is made
; into a key with PBKDF2. Every machine of a network has the same
; one, put there by hand, it is never sent. An install makes a machine
; one of its own. With no such file a machine has no links at all, the
; Net service makes none, service/net/app_impl.lisp.
;
; The proof. Each end sends a word, the version of the link it speaks,
; 16 bytes it has just made up, a nonce, a share of X25519 it has just
; made, and a byte: of the end that dialled, whether the link is to be
; sealed. The word and the version are read first, and an end of another,
; or of before there were versions, is closed on as soon as they are
; heard. Each then sends an HMAC SHA-256, under the key, of the word and
; the version, which end it is, both nonces, both shares and that byte.
; Each checks the other's. The nonces are new
; every time, so an answer heard once is no use again, and the two
; ends say different things, so one's answer is no use sent back to
; it. And one in between who has the key can not put shares of its
; own in, nor say the link is not to be sealed: the proof is of them.
;
; Sealed. A link the dialler made to a machine outside the private
; addresses, over the internet, is sealed: each message on it is
; encrypted and signed, ChaCha20-Poly1305, service/net/link.vp, each
; way its own key, made from the X25519 secret the shares make and
; the key, with HKDF, and its own count for the nonce, so one taken off
; the wire and sent again is not taken. A message that is not right
; drops the link. The keys are new each link: the key found later
; does not open what went before. A link inside, on the LAN, is not
; sealed, and costs nothing more.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/crypto/sha256.inc")
(import "lib/crypto/pbkdf2.inc")
(import "lib/crypto/random.inc")
(import "lib/crypto/x25519.inc")
(import "lib/crypto/hkdf.inc")
(import "lib/crypto/same.inc")

(ffi "service/net/lisp_connect" net-connect)
(ffi "service/net/lisp_listen" net-listen)
(ffi "service/net/lisp_accept" net-accept)
(ffi "service/net/lisp_send" net-send)
(ffi "service/net/lisp_recv" net-recv)
(ffi "service/net/lisp_close" net-close)
(ffi "service/net/lisp_poll" net-poll)

(defq +door_key_file "mesh_key" +door_word "CLDOOR3" +door_nonce_size 16
	+door_share_size 32 +door_tag_size 32 +door_wait 5000000)

;the version of the link: of all that goes over it once it is through the
;door, docs/ai_digest/link_protocol.md. One more each time any of that is
;changed, so two machines that would not understand each other do not link.
;What is heard first, the word and it, and what comes after, the nonce, the
;share and the byte of the seal
(defq +door_version 1 +door_head (cat +door_word (char +door_version 4))
	+door_rest_size (+ +door_nonce_size +door_share_size 1))

;32 bytes of nothing, the keys of a link that is not sealed, and the
;secret of a share of a small order
(defq +door_nothing (apply (const cat) (map (lambda (_) (char 0 +long_size)) (range 0 4))))

(defun door-outside? (host)
	; (door-outside? host) -> :t | :nil
	;is a host dialled outside: not an address of the private ranges, nor
	;loopback, nor a name of the LAN. A link to it is sealed
	(defq host (to-lower host) parts (split host "."))
	(cond
		((eql host "localhost") :nil)
		((some (# (ends-with %0 host)) '(".local" ".lan" ".home" ".internal")) :nil)
		;IPv6, loopback, link local, unique local
		((find ":" host)
			(notany (# (starts-with %0 host)) '("::1" "fe80:" "fc" "fd" "[::1]" "[fe80:" "[fc" "[fd")))
		((and (= (length parts) 4) (every (# (and (nempty? %0) (every (lambda (c) (find c "0123456789")) %0))) parts))
			(bind '(a b &ignore) (map (const str-as-num) parts))
			(not (or (= a 10) (= a 127) (and (= a 172) (<= 16 b 31)) (and (= a 192) (= b 168))
				(and (= a 169) (= b 254)))))
		(:t)))

(defun door-key (&optional file)
	; (door-key [file]) -> :nil | key
	;the key of this machine, 32 bytes, :nil if it has none
	(when (defq text (load (ifn file +door_key_file)))
		(setq text (trim text (const (char-class " \t\r\n"))))
		(cond
			((eql text "") :nil)
			((and (= (length text) 64) (notany (# (not (find %0 "0123456789abcdefABCDEF"))) text))
				(hex-decode (to-upper text)))
			(:t (pbkdf2-sha256 text "ChrysaLisp mesh key" 100000 32)))))

(defun door-tag (key dialler said)
	; (door-tag key dialler said) -> str
	;what an end says to prove it has the key, the end that dialled or the
	;end that listened, of what both said
	(hmac-sha256 key (cat +door_head (if dialler "D" "L") said)))

(defun door-send (handle data until)
	; (door-send handle data until) -> :nil | :t
	;all of it sent, by the time given
	(defq at 0 ok :t)
	(while (and ok (< at (length data)))
		(defq sent (net-send handle (slice data at -1)))
		(cond
			((not (num? sent)) (setq ok :nil))
			((< sent 0) (setq ok :nil))
			((> sent 0) (setq at (+ at sent)))
			((> (pii-time) until) (setq ok :nil))
			(:t (task-sleep 1000))))
	ok)

(defun door-recv (handle size until)
	; (door-recv handle size until) -> :nil | str
	;that many bytes, by the time given, :nil if the other end goes or is
	;too slow
	(defq got "" ok :t)
	(while (and ok (< (length got) size))
		(defq part (net-recv handle (- size (length got))))
		(cond
			((not part) (setq ok :nil))
			((nempty? part) (setq got (cat got part)))
			((> (pii-time) until) (setq ok :nil))
			(:t (task-sleep 1000))))
	(if ok got))

(defun door-shake (handle key dialler sealed &optional wait heard)
	; (door-shake handle key dialler sealed [wait heard]) -> :nil | (sealed send recv)
	;prove to the other end of a connection that this one has the key,
	;and have it prove the same. :nil if either did not. Else whether the
	;link is sealed, as the end that dialled said, and its two keys, the
	;one this end seals with and the one it opens with. Closed on for its
	;word or version, what it said of them is pushed on heard, a list
	(defq until (+ (pii-time) (ifn wait +door_wait)) mine (random-bytes +door_nonce_size)
		secret (random-bytes 32) share (x25519-public secret) theirs :nil)
	(and (door-send handle (cat +door_head mine share (char (if (and dialler sealed) 1 0))) until)
		;of this version, or closed on now, and what it said kept
		(defq head (door-recv handle (length +door_head) until))
		(or (eql head +door_head) (progn (and heard head (push heard head)) :nil))
		(defq hello (door-recv handle +door_rest_size until))
		(setq theirs (slice hello 0 +door_nonce_size))
		;the two nonces must not be the one, or an end could be answered with itself
		(not (crypto-same? mine theirs))
		(defq their_share (slice hello +door_nonce_size (dec +door_rest_size))
			seal (if dialler sealed (= (code hello 1 (dec +door_rest_size)) 1))
			said (if dialler (cat mine theirs share their_share) (cat theirs mine their_share share))
			said (cat said (char (if seal 1 0))))
		(door-send handle (door-tag key dialler said) until)
		(defq tag (door-recv handle +door_tag_size until))
		(crypto-same? tag (door-tag key (not dialler) said))
		(cond
			((not seal) (list :nil +door_nothing +door_nothing))
			;a share that makes nothing, of a small order, is no secret
			((crypto-same? (defq shared (x25519 secret their_share)) +door_nothing) :nil)
			(:t (defq prk (hkdf-extract (slice said 0 (* 2 +door_nonce_size)) (cat shared key))
					to_l (hkdf-expand prk "ChrysaLisp link, dialler to listener" 32)
					to_d (hkdf-expand prk "ChrysaLisp link, listener to dialler" 32))
				(if dialler (list :t to_l to_d) (list :t to_d to_l))))))

(defun door-carry (handle shook)
	; (door-carry handle shook)
	;a connection that is through the door is a link, the tasks that carry
	;mail over it are started, told whether it is sealed and its keys
	(bind '(seal send recv) shook)
	(defq child (open-child "service/net/conn" +kn_call_pin))
	(if (/= (get-long child 0) 0)
		(mail-send child (cat (char handle +long_size) (char (if seal 1 0) +long_size) send recv))
		(net-close handle)))
