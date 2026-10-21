(report-header "Link wire: what a TCP link sends is what docs/ai_digest/link_protocol.md says, byte by byte")

;this node's end of a link is real, the Net service's door and link tasks;
;the other is a task here with a bare socket, that reads what comes and
;takes it apart by the document, its offsets and sizes written out here as
;numbers, not the code's names for them: a change to the code that the
;document does not have is caught. It says a node of its own, made up, so
;it changes the network for a moment, and is in tests/solo/
(import "service/net/door.inc")
((ffi "service/net/lisp_init"))

(defq lw_key (sha256 "link wire") lw_port 31573 lw_listener (net-listen lw_port)
	lw_reply (mail-mbox) lw_node (random-bytes 16) lw_system (random-bytes 16))
(assert-true "a listener" (> lw_listener 0))

(defun lw-uint (s at) (code s 4 at))
(defun lw-zero? (s) (every (# (= (code %0) 0)) s))

;the other end, tests/solo/link_wire_peer.lisp: dials, goes through the
;door by hand as the document says, and says to this task, a line at a
;time, what it was sent
(defq lw_peer (open-child "tests/solo/link_wire_peer.lisp" +kn_call_pin))
(mail-send lw_peer (join (map (const hex-encode) (list lw_reply lw_key lw_node lw_system)) " "))
(mail-send lw_peer (str lw_port))

;this end, the real one: the door, then the link's own tasks
(defq lw_until (+ (pii-time) (task-timeout 10)) lw_handle 0)
(while (and (<= lw_handle 0) (< (pii-time) lw_until))
	(if (/= 0 (logand (net-poll lw_listener) 1)) (setq lw_handle (net-accept lw_listener)) (task-sleep 5000)))
(assert-true "the other end dials" (> lw_handle 0))
(defq lw_shook (if (> lw_handle 0) (catch (door-shake lw_handle lw_key :nil :nil (task-timeout 5)) :nil)))
(assert-true "and is through the door" lw_shook)
(if lw_shook (door-carry lw_handle lw_shook))

(defun lw-heard (what)
	;what the other end said it was sent, of that kind
	(defq got (mail-read-timeout lw_reply (task-timeout 10)))
	(and got (starts-with (cat what " ") got) (hex-decode (slice got (inc (length what)) -1))))

;1. the hello
(defq lw_hello (lw-heard "hello"))
(assert-eq "the hello is 60 bytes" 60 (length (ifn lw_hello "")))
(when (= (length (ifn lw_hello "")) 60)
	(assert-eq "0 to 6, the word" "CLDOOR3" (slice lw_hello 0 7))
	(assert-eq "7 to 10, the version, a uint" 1 (lw-uint lw_hello 7))
	(assert-true "11 to 26, the nonce, not nothing" (not (lw-zero? (slice lw_hello 11 27))))
	(assert-true "27 to 58, the share, not nothing" (not (lw-zero? (slice lw_hello 27 59))))
	(assert-eq "59, the listener's seal byte is 0" 0 (code lw_hello 1 59)))
;4. and 5. the proof
(assert-eq "the proof is HMAC-SHA256 of the first 11 bytes, L and what was said" "proof right"
	(ifn (mail-read-timeout lw_reply (task-timeout 10)) ""))

;the ping
(defq lw_ping (lw-heard "ping"))
(assert-eq "the first ping is 48 bytes" 48 (length (ifn lw_ping "")))
(when (= (length (ifn lw_ping "")) 48)
	(assert-eq "0, the wire length, 40" 40 (lw-uint lw_ping 0))
	(assert-eq "4, the wire type, 1, a ping" 1 (lw-uint lw_ping 4))
	(assert-eq "8 to 23, the node id of the sender" (task-nodeid) (slice lw_ping 8 24))
	(assert-eq "24 to 39, its system id" (cat (system-id)) (slice lw_ping 24 40))
	(assert-true "40, its task count, a uint" (< (lw-uint lw_ping 40) 100000))
	(assert-eq "44 to 47, the pad, 0" (char 0 4) (slice lw_ping 44 48)))

;a fragment: mail from here to the made up node goes over this link
(lw-heard "pinged")
(defq lw_body "a fragment, on the wire" lw_to (cat (char 7 8) lw_node) lw_t0 (pii-time))
;its ping is taken in a moment after it is read
(task-sleep 200000)
(mail-send lw_to lw_body)
(defq lw_frag (lw-heard "frag"))
(assert-eq "a fragment is 72 bytes and its data" (+ 72 (length lw_body)) (length (ifn lw_frag "")))
(when (= (length (ifn lw_frag "")) (+ 72 (length lw_body)))
	(assert-eq "0, the wire length, 64 and the data" (+ 64 (length lw_body)) (lw-uint lw_frag 0))
	(assert-eq "4, the wire type, 2, a fragment" 2 (lw-uint lw_frag 4))
	(assert-eq "8 to 31, dest, the net id it was sent to" lw_to (slice lw_frag 8 32))
	;a mail-send of Lisp gives none, and the wire has what the message had
	(assert-eq "32 to 55, src, the net id it is from, here none" (cat (char 0 8) (char 0 8) (char 0 8)) (slice lw_frag 32 56))
	(assert-eq "56, the length of this fragment's data" (length lw_body) (lw-uint lw_frag 56))
	(assert-eq "60, its offset in the message" 0 (lw-uint lw_frag 60))
	(assert-eq "64, the whole message's length, 0 for one of one fragment" 0 (lw-uint lw_frag 64))
	(assert-eq "68 to 71, the pad, 0" (char 0 4) (slice lw_frag 68 72))
	(assert-eq "72 on, the data, as it was sent" lw_body (slice lw_frag 72 -1)))

(net-close lw_listener)
