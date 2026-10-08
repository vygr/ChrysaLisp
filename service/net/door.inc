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
; one, put there by hand, it is never sent. With no such file a link
; is as it always was, open, and the two kinds do not mix, a machine
; with a key and one without do not link.
;
; The proof. Each end sends a word and 16 bytes it has just made
; up, a nonce. Each then sends an HMAC SHA-256, under the key, of
; which end it is and both nonces. Each checks the other's. The
; nonces are new every time, so an answer heard once is no use
; again, and the two ends say different things, so one's answer is
; no use sent back to it.
;
; What it does not do. What then goes over the link is not hidden,
; and not signed. One who can read the wire can read the mail, and
; one who can change the wire, after the proof, can change it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/crypto/sha256.inc")
(import "lib/crypto/pbkdf2.inc")
(import "lib/crypto/random.inc")

(ffi "service/net/lisp_connect" net-connect)
(ffi "service/net/lisp_listen" net-listen)
(ffi "service/net/lisp_accept" net-accept)
(ffi "service/net/lisp_send" net-send)
(ffi "service/net/lisp_recv" net-recv)
(ffi "service/net/lisp_close" net-close)
(ffi "service/net/lisp_poll" net-poll)

(defq +door_key_file "mesh_key" +door_word "CLDOOR1" +door_nonce_size 16
	+door_hello_size 23 +door_tag_size 32 +door_wait 5000000)

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

(defun door-tag (key dialler dial_nonce listen_nonce)
	; (door-tag key dialler dial_nonce listen_nonce) -> str
	;what an end says to prove it has the key, the end that dialled or the
	;end that listened
	(hmac-sha256 key (cat +door_word (if dialler "D" "L") dial_nonce listen_nonce)))

(defun door-same? (a b)
	; (door-same? a b) -> :nil | :t
	;are two strs the same, every byte looked at, same or not
	(when (= (length a) (length b))
		(defq differ 0)
		(each (# (setq differ (logior differ (logxor (code %0) (code %1))))) a b)
		(= differ 0)))

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

(defun door-shake (handle key dialler &optional wait)
	; (door-shake handle key dialler [wait]) -> :nil | :t
	;prove to the other end of a connection that this one has the key,
	;and have it prove the same. :t only if both did
	(defq until (+ (pii-time) (ifn wait +door_wait)) mine (random-bytes +door_nonce_size) theirs :nil)
	(and (door-send handle (cat +door_word mine) until)
		(defq hello (door-recv handle +door_hello_size until))
		(eql (slice hello 0 (length +door_word)) +door_word)
		(setq theirs (slice hello (length +door_word) -1))
		;the two nonces must not be the one, or an end could be answered with itself
		(not (door-same? mine theirs))
		(defq dial_nonce (if dialler mine theirs) listen_nonce (if dialler theirs mine))
		(door-send handle (door-tag key dialler dial_nonce listen_nonce) until)
		(defq tag (door-recv handle +door_tag_size until))
		(door-same? tag (door-tag key (not dialler) dial_nonce listen_nonce))
		:t))

(defun door-carry (handle)
	; (door-carry handle)
	;a connection that is through the door is a link, the tasks that carry
	;mail over it are started
	(defq child (open-child "service/net/conn" +kn_call_pin))
	(if (/= (get-long child 0) 0)
		(mail-send child (cat (char handle +long_size) (char 0 +long_size)))
		(net-close handle)))
