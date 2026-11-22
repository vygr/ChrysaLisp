(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "./app.inc")

;The fetcher of the Jukebox, a task of its own on a node not the window's,
;so the window goes on being drawn while a module or a folder's listing
;comes, over https by lib/net/tls.inc. Its connection to a host is kept
;from one fetch to the next, lib/net/http.inc: a new one's handshake, and
;the check of who the server is, is most of the time of a fetch, seconds
;on a slow machine.
;
;Told by the window, each message the mailbox to answer on, then:
;	F url		fetch it; of those waiting, only the last
;	K			the window is there
;	Q			go
;It answers a fetch on the mailbox it came with, a new one for each, so
;what is said of one no longer wanted goes nowhere:
;	% got total		how far it has got, total 0 if not said
;	= bytes			its bytes
;	! why			it could not have them
;and it goes if it hears nothing of the window for a while, ./app.inc

(defq +report_every 32768)

(defun fetch (reply url)
	(defq told 0 resp (http-get url :nil :nil (lambda (got total)
			(when (>= (- got told) +report_every)
				(setq told got)
				(mail-send reply (cat "%" (str got) " " (str (ifn total 0))))))))
	(mail-send reply (if (defq why (http-why resp)) (cat "!" why) (cat "=" (http-body-str resp)))))

(defun main ()
	(defq running :t net :nil)
	(while (and running (defq msg (mail-read-timeout (task-mbox) +orphan)))
		(unless net (ensure-net-service) (setq net :t))
		;of what is waiting, only the last fetch asked for
		(defq want :nil)
		(while msg
			(defq kind (if (> (length msg) +net_id_size) (elem-get msg +net_id_size) ""))
			(cond
				((eql kind "F") (setq want msg))
				((eql kind "Q") (setq running :nil want :nil)))
			(setq msg (if (mail-waiting? (task-mbox)) (mail-read (task-mbox)))))
		(if want (fetch (slice want 0 +net_id_size) (slice want (inc +net_id_size) -1)))))
