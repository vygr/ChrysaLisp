;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The gate, a listener for links on a machine that has a key. It is
; sent the key, 32 bytes, then the port, and then, after a space, a
; mailbox if it is to say whether it has the port: "1", or "0" if
; another has it, another session of this machine most likely. Each
; connection it accepts is given to a door of its own,
; service/net/door.lisp, to prove itself, so one that says nothing
; holds up nobody else. Any mail after the first and it stops
; listening and goes: the key is not the key any more.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./door.inc")

(defun main ()
	(defq msg (mail-read (task-mbox)) key (slice msg 0 32)
		what (split (slice msg 32 -1) " ")
		port (str-as-num (first what)) listener (net-listen port)
		select (list (task-mbox)))
	(if (> (length what) 1) (mail-send (hex-decode (second what)) (if (> listener 0) "1" "0")))
	(when (> listener 0)
		;a listener is not work, a build is not to wait for it
		(task-count -1)
		(while listener
			(cond
				((mail-poll select)
					(net-close listener)
					(setq listener :nil))
				((/= 0 (logand (net-poll listener) 1))
					(when (> (defq handle (net-accept listener)) 0)
						(defq child (open-child "service/net/door.lisp" +kn_call_pin))
						(if (/= (get-long child 0) 0)
							(mail-send child (cat "A" key (str handle)))
							(net-close handle))))
				(:t (task-sleep 10000))))
		(task-count 1)))
