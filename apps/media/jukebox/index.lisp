(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "lib/files/zip.inc")
(import "./library.inc")

;The index of the Jukebox, a task of its own on a node not the window's,
;that knows where each module of Modland is now, from its list of all it
;has, allmods.zip, made each day: a playlist names a module where it was
;years ago, under an artist's folder since renamed, or before it was moved
;into a folder of work with another. The list is kept in downloads/mods/,
;with its date and its lines of the trackers played here, and fetched
;again only when Modland has a newer one. It lasts as long as the window.
;
;Told by the window, the first word its mailbox:
;	L id path	where is the module of this path of pub/modules/ now
;	R key \n path \n ...	where are these, a playlist's, now
;	K			the window is there
;	Q			go
;It tells the window, as the fetcher does, on its mailbox:
;	L id path	it is there now, or L id and no path if not known
;	R key \n path \n ...	each there now, - if not known, or
;				as asked if there is no list to look in
;and it goes if it hears nothing of the window for a while

(defq +orphan 5000000 +zip (kept-path +allmods)
	+date (cat +zip ".date") +played (cat +zip ".played"))

(defun allmods-fresh ()
	; (allmods-fresh) -> :nil | str
	;the paths of the list of the trackers played here: fetched again if
	;Modland's is newer than the one kept, or there is none, else the one
	;kept, and :nil if there is none to be had
	(ensure-net-service)
	(defq date (load +date) played (load +played)
		resp (http-get +allmods (if (and date played) (pmap :if-modified-since date) (pmap))))
	(cond
		((and resp (= (pfind resp :status) 200)
				(defq data (http-body-str resp)) (defq entry (zip-find (zip-list data) "allmods.txt"))
				(zip-read data entry (defq out (string-stream (str-alloc 1048576)))))
			(setq played (allmods-played (str out)))
			(save data +zip)
			(save played +played)
			(save (ifn (pfind (pfind resp :headers) :last-modified) "") +date)
			played)
		(played)))

(defun main ()
	(when (defq app (mail-read-timeout (task-mbox) +orphan))
		(defq running :t index (if (defq played (allmods-fresh)) (allmods-index played)))
		(while (and running (defq msg (mail-read-timeout (task-mbox) +orphan)))
			(case (first msg)
				("Q" (setq running :nil))
				("L" (defq gap (ifn (find " " msg) (length msg)) id (slice msg 1 gap)
						path (if index (allmods-resolve index (slice msg (min (length msg) (inc gap)) -1))))
					(mail-send app (cat "L" id (if path (cat " " path) ""))))
				("R" (defq lines (split (slice msg 1 -1) "\n"))
					(mail-send app (cat "R" (join (map! (# (if index (ifn (allmods-resolve index %0) "-") %0))
						(list lines) 1 (length lines) (list (first lines))) "\n"))))))))
