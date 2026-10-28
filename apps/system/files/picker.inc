;;;;;;;;;;;;;
; file picker
;;;;;;;;;;;;;

(import "lib/collections/tree.inc")
(import "service/lock/app.inc")

;what the picker keeps for each user, of every app: the last file taken
;and the folders taken from, newest first
(defq +places_version 1)

(defun file-pick (reply title &optional root kinds save app names places)
	; (file-pick reply title [root kinds save app names places]) -> picker
	;the file picker opened, a window of its own, apps/system/files/child.lisp:
	;at root, a folder or a file to start at, the tree's top if not given,
	;listing the files of kinds, the ends of names as ".cwb .md", all if
	;not given; to save, a name may be one not there; app, a name the places
	;the app took files from are kept by, for the user, and shown, it opens
	;at the last if root is not said, none kept if not given; names, the app
	;opens what it is given by (name-stream), lib/files/names.inc, so the
	;objects of the store are shown too, store:path; places, a list of the
	;folders of the app's own things, shown after the tree and the user's
	;folder, the store and those last taken from. It answers the once,
	;to the mailbox reply, the path taken or "" for none, and goes. The
	;picker's task, to be told "" if it is to go before it answers
	(defq picker (open-child "apps/system/files/child.lisp" +kn_call_pin))
	;each field something, (split) leaves out an empty one: * all kinds, - no app
	(mail-send picker (cat reply title "|" (ifn root ".") "|" (if (nempty? (ifn kinds "")) kinds "*")
		"|" (if save "save" "open") "|" (if (nempty? (ifn app "")) app "-") "|" (if names "names" "-")
		"|" (if (nempty? (ifn places '())) (join places "\t") "-")))
	picker)

(defun file-places-file ()
	; (file-places-file) -> path
	(cat (get :home *env*) "places.tre"))

(defun places-load ()
	; (places-load) -> :nil | tree
	;the user's places of every app, :nil if none kept
	(defq file (file-places-file) tree (if (/= (age file) 0)
		(with-read-lock file (tree-load (file-stream file)))))
	(and tree (eql (. tree :find :version) +places_version) tree))

(defun places-of (app)
	; (places-of app) -> (last_file folder ...)
	(ifn (and app (defq tree (places-load)) (. tree :find (sym (cat ":" app)))) (list "")))

(defun file-pick-last (app)
	; (file-pick-last app) -> :nil | path
	;the last file the app took by the picker, for the user, if it is
	;there still, so it can show it again as it starts
	(and (nempty? (defq file (first (places-of app)))) (/= (age file) 0) file))
