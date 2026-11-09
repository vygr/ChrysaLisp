;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun answer (path)
	;the path taken, or "" for none, to who asked, kept as the app's last
	;place, and the picker goes
	(if (nempty? path) (places-keep *app* path))
	(mail-send reply_mbox path)
	(setq *running* :nil))

(defun action-close ()
	(answer ""))

(defun action-up ()
	(go-to (folder-above *folder*)))

(defun action-path ()
	;the folder written, if it is one
	(defq folder (folder-of (trim (. *path* :get_text))))
	(if (folder? folder) (go-to folder) (. *note* :set_text (cat "No folder " folder "."))))

(defun action-kinds ()
	(setq *exts* (split (. *kinds* :get_text) " "))
	(go-to *folder*))

(defun action-filter ()
	(setq *pattern* (trim (. *filter* :get_text)))
	(go-to *folder*))

(defun action-ok ()
	;the file named, in this folder. A folder named is gone into. To open,
	;it is to be there; to save, one there is replaced only when asked again
	(defq name (trim (. *name* :get_text)))
	;to save, of one kind, the name of that kind, as what saves it will make it
	(and *save* (= (length *exts*) 1) (nempty? name) (not (ends-with (first *exts*) name))
		(not (folder? (path-in *folder* name)))
		(setq name (cat name (first *exts*))))
	(defq path (if (or (starts-with "/" name) (and *names* (name-in-store name))) name (path-in *folder* name)))
	(cond
		((eql name "") (. *note* :set_text (if *save* "Write a name." "Take a file, or write its name.")))
		((folder? path) (. *name* :set_text "") (go-to (folder-of path)))
		((not *save*) (if (name-size path) (answer path) (. *note* :set_text (cat "No file " name " here."))))
		((or (not (name-size path)) (eql path *replace*)) (answer path))
		(:t (setq *replace* path) (. *note* :set_text (cat name " is here. OK again to replace it.")))))

(defun action-item (n)
	;a folder is gone into; a file taken, and taken again opened
	(defq name (elem-get *items* n))
	(cond
		((ends-with "/" name) (go-to (path-in *folder* (slice name 0 -2))))
		((eql name (trim (. *name* :get_text))) (action-ok))
		((. *name* :set_text name) (. *note* :set_text "") (preview-show (path-in *folder* name)))))
