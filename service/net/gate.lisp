;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The gate, a listener for links on a machine that has a key. It is
; sent the key, 32 bytes, and then the port. Each connection it accepts is given to a door of its
; own, service/net/door.lisp, to prove itself, so one that says
; nothing holds up nobody else.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./door.inc")

(defun main ()
	(defq msg (mail-read (task-mbox)) key (slice msg 0 32)
		port (str-as-num (slice msg 32 -1)) listener (net-listen port))
	(when (> listener 0)
		;a listener is not work, a build is not to wait for it
		(task-count -1)
		(while :t
			(cond
				((/= 0 (logand (net-poll listener) 1))
					(when (> (defq handle (net-accept listener)) 0)
						(defq child (open-child "service/net/door.lisp" +kn_call_pin))
						(if (/= (get-long child 0) 0)
							(mail-send child (cat "A" key (str handle)))
							(net-close handle))))
				(:t (task-sleep 10000))))))
