;the other end of a link for tests/solo/test_link_wire.lisp: a bare socket.
;It dials, goes through the door by hand as docs/ai_digest/link_protocol.md
;says, the formulas its own and not (door-tag), and says to the test, a
;line at a time, what it was sent: the hello, if its proof was right, the
;first ping, and a fragment. It says a node of its own, made up, in a ping,
;so mail for that node comes to it
(import "service/net/door.inc")

(defun main ()
	(bind '(said key node system) (map (# (hex-decode (to-upper %0))) (split (mail-read (task-mbox)) " ")))
	(defq port (str-as-num (mail-read (task-mbox)))
		h (net-connect "127.0.0.1" port)
		;as long as it takes, longer than the suite waits for a module
		until (+ (pii-time) (task-timeout 600)))
	(defun tell (k v) (mail-send said (cat k " " v)))
	(while (and (> h 0) (= 0 (logand (net-poll h) 2)) (< (pii-time) until)) (task-sleep 5000))
	;1. the hello, 60 bytes, this end's own, not sealed
	(defq mine (random-bytes 16) secret (random-bytes 32) share (x25519-public secret))
	(door-send h (cat "CLDOOR3" (char 1 4) mine share (char 0)) until)
	(defq hello (door-recv h 60 until))
	(tell "hello" (hex-encode (ifn hello "")))
	(when hello
		;3. what was said, 97 bytes, and 4. the proof
		(defq said_97 (cat mine (slice hello 11 27) share (slice hello 27 59) (char 0))
			head (slice hello 0 11))
		(door-send h (hmac-sha256 key (cat head "D" said_97)) until)
		(defq theirs (door-recv h 32 until))
		(tell "proof" (if (eql theirs (hmac-sha256 key (cat head "L" said_97))) "right" "wrong"))
		;the link: its first ping, 48 bytes
		(tell "ping" (hex-encode (ifn (door-recv h 48 until) "")))
		;one of this end's, as a node of its own
		(door-send h (cat (char 40 4) (char 1 4) node system (char 0 4) (char 0 4)) until)
		(tell "pinged" "")
		;the fragment the test sends, to mailbox 0x51ab0c of this made up
		;node, one no other mail is to: past the pings that come each
		;second, the parcel before it, and the kernel's own mail to a node
		;it has just heard of. And of every fragment that is a message of
		;one, total 0, how many had a src or an offset that is not 0
		(defq until (+ (pii-time) (task-timeout 600)) frag :nil ones 0 stale 0)
		(while (and (not frag) (< (pii-time) until) (defq wire (door-recv h 8 until)))
			(defq body (door-recv h (code wire 4 0) until))
			(when (and body (= (code wire 4 4) 2))
				(when (= (code body 4 56) 0)
					(setq ones (inc ones))
					(unless (and (eql (slice body 24 48) (cat (char 0 8) (char 0 8) (char 0 8))) (= (code body 4 52) 0))
						(setq stale (inc stale))))
				(if (eql (slice body 0 8) (char 0x51ab0c 8)) (setq frag (cat wire body)))))
		(tell "frag" (hex-encode (ifn frag "")))
		(tell "ones" (cat (str ones) " " (str stale))))
	(task-sleep 500000)
	(if (> h 0) (net-close h)))
