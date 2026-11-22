(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "./app.inc")

;The fetcher of the Jukebox, a task of its own on a node not the window's,
;so the window goes on being drawn while a module or a folder's listing
;comes, over https by lib/net/tls.inc. It lasts as long as the window, and
;its connection to a host is kept from one fetch to the next,
;lib/net/http.inc: a new one's handshake, and the check of who the server
;is, is most of the time of a fetch, seconds on a slow machine.
;
;Told by the window, the first word its mailbox:
;	F id url		fetch it; of those waiting, only the last
;	K			the window is there
;	Q			go
;It tells the window, of the fetch of that id:
;	% id got total		how far it has got, total 0 if not said
;	= id bytes		its bytes
;	! id why		it could not have them
;and it goes if it hears nothing of the window for a while

(defq +report_every 32768)

(defun fetch (app id url)
	(defq told 0)
	(defq resp (http-get url :nil :nil (lambda (got total)
			(when (>= (- got told) +report_every)
				(setq told got)
				(mail-send app (cat "%" id " " (str got) " " (str (ifn total 0))))))))
	(mail-send app (if (defq why (http-why resp)) (cat "!" id " " why) (cat "=" id " " (http-body-str resp)))))

(defun main ()
	(when (defq app (mail-read-timeout (task-mbox) +orphan))
		(ensure-net-service)
		(defq running :t)
		(while (and running (defq msg (mail-read-timeout (task-mbox) +orphan)))
			;of what is waiting, only the last fetch asked for
			(defq want :nil)
			(while msg
				(cond
					((eql (first msg) "F") (setq want msg))
					((eql (first msg) "Q") (setq running :nil want :nil)))
				(setq msg (if (mail-poll (list (task-mbox))) (mail-read (task-mbox)))))
			(when want
				(defq gap (find " " want))
				(fetch app (slice want 1 gap) (slice want (inc gap) -1))))))
