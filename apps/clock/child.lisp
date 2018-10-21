;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(defun make-time ()
	(defq sec (div (time) 1000000) seconds (mod sec 60)
		minutes (mod (div sec 60) 60) hours (mod (div sec 60 60) 24))
	(cat (pad hours 2 "0") ":" (pad minutes 2 "0") ":" (pad seconds 2 "0")))

;read args from parent
(bind '(display) (mail-mymail))

;while not told to quit
(until (mail-trymail)
	(set display 'text (make-time))
	(view-dirty display)
	(task-sleep 1000000))
