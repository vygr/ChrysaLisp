;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; One connection at the door, service/net/door.inc. It is sent what
; to do, a D to dial and prove, or an A to prove on a connection a
; gate has accepted, then the key, 32 bytes, then the host:port or
; the handle. Through the door it is a link, and if not it is closed.
; The key comes in the message, from the Net service, which worked
; it out the once, from a passphrase that takes a while.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./door.inc")

(defun main ()
	(defq msg (mail-read (task-mbox)) kind (slice msg 0 1) key (slice msg 1 33)
		what (slice msg 33 -1) handle 0)
	(when (= (length key) 32)
		(cond
			((eql kind "A") (setq handle (str-as-num what)))
			(:t ;dial, the port is after the last :
				(defq at (rfind ":" what) host (if at (slice what 0 (dec at)) what)
					port (if at (str-as-num (slice what at -1)) 3333)
					until (+ (pii-time) +door_wait))
				(setq handle (net-connect host port))
				;wait for it to connect
				(while (and (> handle 0) (= 0 (logand (defq flags (net-poll handle)) 2)))
					(cond
						((or (/= 0 (logand flags 4)) (> (pii-time) until))
							(net-close handle) (setq handle 0))
						(:t (task-sleep 10000))))))
		(when (> handle 0)
			(if (catch (door-shake handle key (eql kind "D")) :nil)
				(door-carry handle)
				(net-close handle)))))
