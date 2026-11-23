(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "lib/files/zip.inc")
(import "lib/text/dictionary.inc")
(import "./app.inc")
(import "./library.inc")

;The index of the Jukebox, a task of its own on a node not the window's,
;that knows where each module of Modland is now, from its list of all it
;has, allmods.zip, made each day: a playlist names a module where it was
;years ago, under an artist's folder since renamed, or before it was moved
;into a folder of work with another. The list is kept in downloads/mods/,
;with its date and its lines of the trackers played here, and fetched
;again only when Modland has a newer one. It lasts as long as the window.
;
;Told by the window, each message the mailbox to answer on, then:
;	L path		where is the module of this path of pub/modules/ now
;	R key \n path \n ...	where are these, a playlist's, now
;	S \n :prefix \n :filter	the modules whose names start so, of all
;				Modland has, as the filter, a regexp or plain text,
;				leaves them, of what is waiting only the last
;	K			the window is there, and this is its mailbox for
;				what it did not ask for
;	Q			go
;It answers on the mailbox each came with, a new one for each lookup and
;each search, so what is said of one no longer wanted goes nowhere:
;	L path		it is there now, or L and no path if not known
;	R key \n path \n ...	each there now, - if not known, or
;				as asked if there is no list to look in
;	S total \n path \n ...	the first so many found of all there are
;and tells the window, on the mailbox of its K:
;	E \n url \n ...	the playlists, and folders of them, of nothing it
;				has played here, once it has looked at them all
;and it goes if it hears nothing of the window for a while, ./app.inc

(defq +zip (kept-path +allmods)
	+date (cat +zip ".date") +played (cat +zip ".played")
	;the trackers the paths kept are of: others played here now, they are
	;made again from the list kept
	+kinds (cat +zip ".kinds") +kinds_now (join +modland_kinds ",")
	;the window's mailbox for what it did not ask for, the index, is it to go
	;on, and the playlists of nothing played here, once known and once told
	*app* :nil *index* :nil *running* :t *empty* :nil *empty_told* :nil
	;the names of all, (Dictionary), a search asked for, and the most of
	;what it finds that is sent
	*names* :nil *search* :nil +search_max 2000)

(defun allmods-zip-played (data)
	; (allmods-zip-played data) -> :nil | str
	;the paths of the trackers played here of the bytes of allmods.zip
	(and (defq entry (zip-find (zip-list data) "allmods.txt"))
		(zip-read data entry (defq out (string-stream (str-alloc 1048576))))
		(allmods-played (str out))))

(defun allmods-fresh ()
	; (allmods-fresh) -> :nil | str
	;the paths of the list of the trackers played here: fetched again if
	;Modland's is newer than the one kept, or there is none, else the one
	;kept, and :nil if there is none to be had. The paths kept made again
	;from the list kept if they are of other trackers than those now
	(ensure-net-service)
	(defq date (load +date) played (if (eql (load +kinds) +kinds_now) (load +played)))
	(when (and (not played) (defq data (load +zip)) (setq played (allmods-zip-played data)))
		(save played +played)
		(save +kinds_now +kinds))
	(defq resp (http-get +allmods (if (and date played) (pmap :if-modified-since date) (pmap))))
	(cond
		((and resp (= (pfind resp :status) 200)
				(defq data (http-body-str resp)) (defq fresh (allmods-zip-played data)))
			(save data +zip)
			(save fresh +played)
			(save +kinds_now +kinds)
			(save (ifn (pfind (pfind resp :headers) :last-modified) "") +date)
			fresh)
		(played)))

(defun playlist-has? (index url)
	; (playlist-has? index url) -> :t | :nil
	;has a playlist a module played here that Modland has, its copy kept,
	;or fetched and kept if there is none
	(unless (defq text (load (kept-path url)))
		(and (defq resp (http-get url)) (= (pfind resp :status) 200)
			(save (setq text (http-body-str resp)) (kept-path url))))
	(and text (some (# (if index (allmods-resolve index (modland-path (second %0))) :t)) (playlist-read text))))

(defun search-run ()
	;the search asked for last: the names of all that start with its
	;prefix, in lower case, each of their paths the filter leaves, a regexp
	;ignoring case or plain text if it is not one, the first +search_max
	;sent and how many there are. Left for the next if another is asked
	;its lines are after the mailbox, which is bytes, a newline among them
	;now and then
	(defq reply (slice *search* 0 +net_id_size))
	(bind '(prefix pattern) (split (slice *search* (inc +net_id_size) -1) "\n"))
	(setq *search* :nil prefix (to-lower (rest prefix)) pattern (to-lower (rest pattern)))
	(defq leaves (name-filter pattern) total 0 seen 0 out (list)
		words (if *names* (. *names* :find_matches prefix) (list)))
	(when (every (lambda (word)
				(each (lambda (path)
						(when (name-filter? leaves path)
							(setq total (inc total))
							(if (<= total +search_max) (push out path))))
					(. (first *index*) :find word))
				;a newer search waiting, this one left
				(or (/= 0 (% (setq seen (inc seen)) 4096)) (not (mail-waiting? (task-mbox)))))
			words)
		(mail-send reply (cat "S" (str total) "\n" (join out "\n")))))

(defun empty-tell ()
	;the playlists of nothing played here told the window, once they are
	;known and it has said where
	(when (and *app* *empty* (not *empty_told*))
		(setq *empty_told* :t)
		(mail-send *app* (cat "E\n" (join *empty* "\n")))))

(defun answer (msg)
	;what the window asked, answered on the mailbox it came with; :nil once
	;it is told to go
	(when (> (length msg) +net_id_size)
		(defq reply (slice msg 0 +net_id_size) body (slice msg (inc +net_id_size) -1))
		(case (elem-get msg +net_id_size)
			("Q" (setq *running* :nil))
			("K" (setq *app* reply) (empty-tell))
			("L" (defq path (if *index* (allmods-resolve *index* body)))
				(mail-send reply (cat "L" (ifn path ""))))
			("S" (setq *search* msg))
			("R" (defq lines (split body "\n"))
				(mail-send reply (cat "R" (join (map! (# (if *index* (ifn (allmods-resolve *index* %0) "-") %0))
					(list lines) 1 (length lines) (list (first lines))) "\n"))))))
	*running*)

(defun answer-waiting ()
	; (answer-waiting) -> :t | :nil
	;what the window has asked meanwhile answered, the last search of it
	;once none is waiting, :nil once told to go
	(while (and *running* (mail-waiting? (task-mbox)))
		(answer (mail-read (task-mbox))))
	(and *running* *search* (search-run))
	*running*)

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

(defun main ()
	;the first that comes is kept till there is an index to answer it by
	(when (defq msg (mail-read-timeout (task-mbox) +orphan))
		(when (defq played (allmods-fresh))
			(setq *index* (allmods-index played) *names* (Dictionary 16381))
			(. (first *index*) :each (lambda (name &ignore) (. *names* :insert_word name)))
			(. *names* :sort))
		(answer msg)
		(answer-waiting)
		(setq *empty* (playlists-empty *index*))
		(if *running* (empty-tell))
		(while (and *running* (defq msg (mail-read-timeout (task-mbox) +orphan)))
			(answer msg)
			(answer-waiting))))
