;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A port of a test's own. One listener has a port, so a suite beside
; this one, or a session left up, has it and this test would not. A
; test takes the first free of those from the one given, the port is
; not what it tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/net/door.inc")

(defun test-listen (port)
	; (test-listen port) -> (port listener)
	;listening on that port or the next free, a listener of 0 if 64 of
	;them are taken
	(defq listener 0 tries 64)
	(while (and (<= (setq listener (net-listen port)) 0) (> (-- tries) 0))
		(++ port))
	(list port listener))
