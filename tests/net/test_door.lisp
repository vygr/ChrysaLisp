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
(assert-eq "a tag is 32 bytes" 32 (length (door-tag dr_key :t (cat dr_n1 dr_n2))))
(assert-true "the two ends say different things"
	(not (eql (door-tag dr_key :t (cat dr_n1 dr_n2)) (door-tag dr_key :nil (cat dr_n1 dr_n2)))))
(assert-true "another key says another thing"
	(not (eql (door-tag dr_key :t (cat dr_n1 dr_n2)) (door-tag dr_other :t (cat dr_n1 dr_n2)))))
(assert-true "other nonces, another thing"
	(not (eql (door-tag dr_key :t (cat dr_n1 dr_n2)) (door-tag dr_key :t (cat dr_n2 dr_n1)))))
(assert-true "sealed or not, another thing"
	(not (eql (door-tag dr_key :t (cat dr_n1 dr_n2 (char 0))) (door-tag dr_key :t (cat dr_n1 dr_n2 (char 1))))))

;which hosts a link to is sealed
(test-cases
	(door-outside? "baydrift.co.uk") :t
	(door-outside? "8.8.8.8") :t
	(door-outside? "172.32.0.1") :t
	(door-outside? "2001:db8::1") :t
	(door-outside? "localhost") :nil
	(door-outside? "127.0.0.1") :nil
	(door-outside? "10.1.2.3") :nil
	(door-outside? "172.16.0.1") :nil
	(door-outside? "172.31.255.255") :nil
	(door-outside? "192.168.1.188") :nil
	(door-outside? "169.254.0.1") :nil
	(door-outside? "::1") :nil
	(door-outside? "fe80::1") :nil
	(door-outside? "fd00::1") :nil
	(door-outside? "Pi4.local") :nil
	(door-outside? "box.lan") :nil
	(door-outside? "nas.home") :nil
	(door-outside? "mesh.internal") :nil)
(test-cases
	(door-same? "abc" "abc") :t
	(door-same? "abc" "abd") :nil
	(door-same? "abc" "ab") :nil
	(door-same? "" "") :t)

;the proof itself, over a real connection, this task listens and a child dials
;the port is below 32768: Linux gives a connection going out a port of its
;own from 32768 to 60999, macOS from 49152, and on the Pi with the suite
;beside it the test failed now and then as if one of those had it
(defq dr_port 31571 dr_listener (net-listen dr_port) dr_reply (mail-mbox) dr_said :nil)
(assert-true "a listener" (> dr_listener 0))

(defun dr-dial (code)
	;a child that connects, runs the code with the connection as h, and says what came of it
	(open-task (str `(progn
			(import "service/net/door.inc")
			(defq h (net-connect "127.0.0.1" ,dr_port) until (+ (pii-time) (task-timeout 3)))
			(while (and (= 0 (logand (net-poll h) 2)) (< (pii-time) until)) (task-sleep 5000))
			(defq r (catch ,code :nil))
			(mail-send (hex-decode ,(hex-encode dr_reply))
				(cond ((list? r) (cat "yes" (if (first r) "S" "U") (hex-encode (second r)) (hex-encode (third r))))
					(r "yes") ("no")))
			(task-sleep 300000)
			(net-close h)))
		(task-nodeid) +kn_call_pin 0 (mail-mbox)))

(defun dr-said (r)
	;what came of a shake, through or not, sealed or not and the keys
	(cond ((list? r) (cat "yes" (if (first r) "S" "U") (hex-encode (second r)) (hex-encode (third r))))
		(r "yes") ("no")))

(defun dr-meet (key code &optional wait)
	;what the listener made of a connection, and what the child did, (listener
	;child), through or not, and in dr_said all they said
	(dr-dial code)
	;the child is a task that starts and imports before it dials, which on
	;a Pi with the suite beside it has taken over 3s, and one that dials
	;late is taken by the next test
	(defq until (+ (pii-time) (task-timeout 15)) handle 0)
	(while (and (<= handle 0) (< (pii-time) until))
		(if (/= 0 (logand (net-poll dr_listener) 1)) (setq handle (net-accept dr_listener)) (task-sleep 5000)))
	(defq mine (if (> handle 0) (dr-said (catch (door-shake handle key :nil :nil (ifn wait (task-timeout 3))) :nil)) "none")
		theirs (ifn (mail-read-timeout dr_reply (task-timeout 5)) "silent"))
	(if (> handle 0) (net-close handle))
	(setq dr_said (list mine theirs))
	(map (# (slice %0 0 (min 3 (length %0)))) dr_said))

(assert-list-eq "the same key, both ends are through" '("yes" "yes")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :t :nil (task-timeout 3))))
(assert-list-eq "and again, the nonces are new each time" '("yes" "yes")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :t :nil (task-timeout 3))))
(assert-list-eq "another key, neither end is" '("no" "no")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_other)) :t :nil (task-timeout 3))))
(assert-list-eq "an end that takes the wrong part is not" '("no" "no")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :nil :nil (task-timeout 3))))
(assert-eq "one that says something else is closed on" "no"
	(first (dr-meet dr_key '(progn (door-send h "GET / HTTP/1.0 and a good deal more than a nonce" (+ (pii-time) 1000000)) :nil))))
(assert-eq "one that says nothing is not waited on for ever" "no"
	(first (dr-meet dr_key '(progn (task-sleep 1500000) :nil) 400000)))
(assert-eq "one that sends the word and then goes" "no"
	(first (dr-meet dr_key '(progn (door-send h "CLDOOR2" (+ (pii-time) 1000000)) :nil) 600000)))
(assert-list-eq "not sealed, the dialler said not" '("yes" "yes")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :t :nil (task-timeout 3))))
(assert-eq "so neither end has keys, two of 32 bytes of nothing" (cat "yesU" (hex-encode (apply (const cat) (map (lambda (_) (char 0 8)) (range 0 8)))))
	(first dr_said))
(assert-list-eq "sealed, the dialler said so" '("yes" "yes")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :t :t (task-timeout 3))))
(bind '(dr_l dr_d) dr_said)
(assert-true "both ends know it is sealed"
	(and (eql (slice dr_l 0 4) "yesS") (eql (slice dr_d 0 4) "yesS")))
(assert-eq "what one seals with the other opens with"
	(slice dr_l 4 68) (slice dr_d 68 132))
(assert-eq "and the other way"
	(slice dr_l 68 132) (slice dr_d 4 68))
(assert-true "each way its own key"
	(not (eql (slice dr_l 4 68) (slice dr_l 68 132))))
(assert-list-eq "sealed again, new keys" '("yes" "yes")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :t :t (task-timeout 3))))
(assert-true "the keys are new each link"
	(not (eql (slice dr_l 4 68) (slice (first dr_said) 4 68))))
(assert-list-eq "a share that makes a secret of nothing is not taken, though the key is right" '("no" "yes")
	(dr-meet dr_key `(progn
		(defq u (+ (pii-time) (task-timeout 3)) k (hex-decode ,(hex-encode dr_key)) n (random-bytes 16) z +door_nothing)
		(door-send h (cat "CLDOOR2" n z (char 1 1)) u)
		(defq hl (door-recv h 56 u) said (cat n (slice hl 7 23) z (slice hl 23 55) (char 1 1)))
		(door-send h (door-tag k :t said) u)
		(door-recv h 32 u))))
(assert-list-eq "and after all that the same key is still through" '("yes" "yes")
	(dr-meet dr_key `(door-shake h (hex-decode ,(hex-encode dr_key)) :t :nil (task-timeout 3))))

(net-close dr_listener)
(undef (env) 'dr_file 'dr_hex 'dr_key 'dr_other 'dr_n1 'dr_n2 'dr_port 'dr_listener 'dr_reply 'dr_said 'dr_l 'dr_d)
