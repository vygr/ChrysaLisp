(import "service/net/app.inc")
(import "lib/net/http.inc")

;The fetching of a module or of a folder's listing, over https by
;lib/net/tls.inc, a task of its own, so the window goes on being drawn
;while it comes. Asked for an address it sends back its bytes, or ! and
;why it could not have them. What is kept the app keeps

(defun child-run (said)
	(defq reply (slice said 0 +net_id_size) url (slice said +net_id_size -1))
	(ensure-net-service)
	(mail-send reply (cond
		((not (defq resp (http-get url))) (cat "!" (ifn (tls-why) "no connection")))
		((/= (pfind resp :status) 200)
			(cat "!" (str (pfind resp :status)) " " (ifn (pfind resp :reason) "")))
		((http-body-str resp)))))

(defun main ()
	;who started it may be gone before it says what is wanted
	(if (defq said (mail-read-timeout (task-mbox) 10000000)) (child-run said)))
