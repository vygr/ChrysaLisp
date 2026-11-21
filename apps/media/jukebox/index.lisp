(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "lib/files/zip.inc")
(import "lib/text/dictionary.inc")
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
;	S id \n :prefix \n :filter	the modules whose names start so, of all
;				Modland has, as the filter, a regexp or plain text,
;				leaves them, of what is waiting only the last
;	K			the window is there
;	Q			go
;It tells the window, as the fetcher does, on its mailbox:
;	L id path	it is there now, or L id and no path if not known
;	R key \n path \n ...	each there now, - if not known, or
;				as asked if there is no list to look in
;	E \n url \n ...	the playlists, and folders of them, of nothing it
;				has played here, once it has looked at them all
;	S id total \n path \n ...	the first so many found of all there are
;and it goes if it hears nothing of the window for a while

(defq +orphan 5000000 +zip (kept-path +allmods)
	+date (cat +zip ".date") +played (cat +zip ".played")
	;the window's mailbox, the index, and is it to go on
	*app* :nil *index* :nil *running* :t
	;the names of all, (Dictionary), a search asked for, and the most of
	;what it finds that is sent
	*names* :nil *search* :nil +search_max 2000)

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

(defun playlist-has? (index url)
	; (playlist-has? index url) -> :t | :nil
	;has a playlist a module played here that Modland has, its copy kept,
	;or fetched and kept if there is none
	(unless (defq text (load (kept-path url)))
		(and (defq resp (http-get url)) (= (pfind resp :status) 200)
			(save (setq text (http-body-str resp)) (kept-path url))))
	(and text (some (# (if index (allmods-resolve index (modland-path (second %0))) :t)) (playlist-read text))))

(defun playlists-empty (index)
	; (playlists-empty index) -> (url ...)
	;the playlists of Modland's with nothing played here, and the folders of
	;them with none that has: each folder's listing read, each playlist's
	;copy kept, (playlist-has?)
	(defq top (cat +modland_pub "playlists/") todo (list top) folders (list) lists (list))
	(while (and (answer-waiting) (defq folder (pop todo)))
		(and (defq resp (http-get folder)) (= (pfind resp :status) 200)
			(each (lambda ((name sub))
					(cond
						(sub (push todo (cat folder name)) (push folders (cat folder name)))
						((ends-with ".m3u" name) (push lists (cat folder name)))))
				(listing-read (http-body-str resp)))))
	;what the window asks meanwhile answered between each, a lookup is
	;not held up by them, the first time 59 fetched
	(defq empty (filter (# (and (answer-waiting) (not (playlist-has? index %0)))) lists))
	(cat empty (filter (lambda (folder)
			(not (some (# (and (starts-with folder %0) (not (find %0 empty)))) lists)))
		folders)))

(defun answer (msg)
	;what the window asked, answered; :nil once it is told to go
	(case (first msg)
		("Q" (setq *running* :nil))
		("L" (defq gap (ifn (find " " msg) (length msg)) id (slice msg 1 gap)
				path (if *index* (allmods-resolve *index* (slice msg (min (length msg) (inc gap)) -1))))
			(mail-send *app* (cat "L" id (if path (cat " " path) ""))))
		("S" (setq *search* msg))
		("R" (defq lines (split (slice msg 1 -1) "\n"))
			(mail-send *app* (cat "R" (join (map! (# (if *index* (ifn (allmods-resolve *index* %0) "-") %0))
				(list lines) 1 (length lines) (list (first lines))) "\n")))))
	*running*)

(defun answer-waiting ()
	; (answer-waiting) -> :t | :nil
	;what the window has asked meanwhile answered, the last search of it
	;once none is waiting, :nil once told to go
	(while (and *running* (mail-poll (list (task-mbox))))
		(answer (mail-read (task-mbox))))
	(and *running* *search* (search-run))
	*running*)

(defun search-run ()
	;the search asked for last: the names of all that start with its
	;prefix, in lower case, each of their paths the filter leaves, a regexp
	;ignoring case or plain text if it is not one, the first +search_max
	;sent and how many there are. Left for the next if another is asked
	(bind '(head prefix pattern) (split (slice *search* 1 -1) "\n"))
	(setq *search* :nil prefix (to-lower (rest prefix)) pattern (to-lower (rest pattern)))
	(defq found (query pattern :nil :t :t) total 0 seen 0 out (list)
		words (if *names* (. *names* :find_matches prefix) (list)))
	(unless (second found) (setq found (query pattern :nil :nil :t)))
	(bind '(engine meta &ignore) found)
	(when (every (lambda (word)
				(each (lambda (path)
						(when (or (eql pattern "") (. engine :match? (to-lower path) meta))
							(setq total (inc total))
							(if (<= total +search_max) (push out path))))
					(. (first *index*) :find word))
				;a newer search waiting, this one left
				(or (/= 0 (% (setq seen (inc seen)) 4096)) (not (mail-poll (list (task-mbox))))))
			words)
		(mail-send *app* (cat "S" head " " (str total) "\n" (join out "\n")))))

(defun main ()
	(when (setq *app* (mail-read-timeout (task-mbox) +orphan))
		(when (defq played (allmods-fresh))
			(setq *index* (allmods-index played) *names* (Dictionary 16381))
			(. (first *index*) :each (lambda (name &ignore) (. *names* :insert_word name)))
			(. *names* :sort))
		(defq empty (playlists-empty *index*))
		(if *running* (mail-send *app* (cat "E\n" (join empty "\n"))))
		(while (and *running* (defq msg (mail-read-timeout (task-mbox) +orphan)))
			(answer msg)
			(answer-waiting))))
