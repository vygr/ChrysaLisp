(report-header "Net: the door of a link, two machines prove they have the key")
(import "service/net/door.inc")
((ffi "service/net/lisp_init"))

;the key of a machine, from its file
(defq dr_file "tests/scratch/door_key_test" dr_hex "00112233445566778899aabbccddeeff00112233445566778899AABBCCDDEEFF")
(save (cat dr_hex (ascii-char 10)) dr_file)
(assert-eq "64 hex digits are the 32 bytes" (hex-decode (to-upper dr_hex)) (door-key dr_file))
(save "  " dr_file)
(assert-eq "a file of nothing is no key" :nil (door-key dr_file))
(assert-eq "no file is no key" :nil (door-key "tests/scratch/door_key_is_not_there"))
(pii-remove dr_file)

;what an end says
(defq dr_key (hex-decode (to-upper dr_hex)) dr_other (sha256 "another key")
	dr_n1 (sha256 "one") dr_n2 (sha256 "two"))
(assert-eq "a tag is 32 bytes" 32 (length (door-tag dr_key :t dr_n1 dr_n2)))
(assert-true "the two ends say different things"
	(not (eql (door-tag dr_key :t dr_n1 dr_n2) (door-tag dr_key :nil dr_n1 dr_n2))))
(assert-true "another key says another thing"
	(not (eql (door-tag dr_key :t dr_n1 dr_n2) (door-tag dr_other :t dr_n1 dr_n2))))
(assert-true "other nonces, another thing"
	(not (eql (door-tag dr_key :t dr_n1 dr_n2) (door-tag dr_key :t dr_n2 dr_n1))))
(test-cases
	(door-same? "abc" "abc") :t
	(door-same? "abc" "abd") :nil
	(door-same? "abc" "ab") :nil
	(door-same? "" "") :t)

;the proof itself, over a real connection, this task listens and a child dials
(defq dr_port 34571 dr_listener (net-listen dr_port) dr_reply (mail-mbox))
(assert-true "a listener" (> dr_listener 0))

(defun dr-dial (code)
	;a child that connects, runs the code with the connection as h, and says what came of it
	(open-task (str `(progn
			(import "service/net/door.inc")
			(defq h (net-connect "127.0.0.1" ,dr_port) until (+ (pii-time) 3000000))
			(while (and (= 0 (logand (net-poll h) 2)) (< (pii-time) until)) (task-sleep 5000))
			(defq r (catch ,code :nil))
			(mail-send (hex-decode ,(hex-encode dr_reply)) (if r "yes" "no"))
			(task-sleep 300000)
			(net-close h)))
		(task-nodeid) +kn_call_pin 0 (mail-mbox)))

(defun dr-meet (key code &optional wait)
	;what the listener made of a connection, and what the child did, (listener child)
	(dr-dial code)
	(defq until (+ (pii-time) 3000000) handle 0)
	(while (and (<= handle 0) (< (pii-time) until))
		(if (/= 0 (logand (net-poll dr_listener) 1)) (setq handle (net-accept dr_listener)) (task-sleep 5000)))
	(defq mine (if (> handle 0) (if (catch (door-shake handle key :nil (ifn wait 3000000)) :nil) "yes" "no") "none")
		theirs (ifn (mail-read-timeout dr_reply 5000000) "silent"))
	(if (> handle 0) (net-close handle))
	(list mine theirs))

(assert-list-eq "the same key, both ends are through" '("yes" "yes")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :t 3000000)))
(assert-list-eq "and again, the nonces are new each time" '("yes" "yes")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :t 3000000)))
(assert-list-eq "another key, neither end is" '("no" "no")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_other)) :t 3000000)))
(assert-list-eq "an end that takes the wrong part is not" '("no" "no")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :nil 3000000)))
(assert-eq "one that says something else is closed on" "no"
	(first (dr-meet dr_key '(progn (door-send h "GET / HTTP/1.0 and a good deal more than a nonce" (+ (pii-time) 1000000)) :nil))))
(assert-eq "one that says nothing is not waited on for ever" "no"
	(first (dr-meet dr_key '(progn (task-sleep 1500000) :nil) 400000)))
(assert-eq "one that sends the word and then goes" "no"
	(first (dr-meet dr_key '(progn (door-send h "CLDOOR1" (+ (pii-time) 1000000)) :nil) 600000)))
(assert-list-eq "and after all that the same key is still through" '("yes" "yes")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :t 3000000)))

(net-close dr_listener)
(undef (env) 'dr_file 'dr_hex 'dr_key 'dr_other 'dr_n1 'dr_n2 'dr_port 'dr_listener 'dr_reply)
