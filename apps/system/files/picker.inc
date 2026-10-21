;;;;;;;;;;;;;
; file picker
;;;;;;;;;;;;;

(defun file-pick (reply title &optional root kinds save app names)
	; (file-pick reply title [root kinds save app names]) -> picker
	;the file picker opened, a window of its own, apps/system/files/child.lisp:
	;at root, a folder or a file to start at, the tree's top if not given,
	;listing the files of kinds, the ends of names as ".cwb .md", all if
	;not given; to save, a name may be one not there; app, a name the places
	;the app took files from are kept by, for the user, and shown, it opens
	;at the last if root is not said, none kept if not given; names, the app
	;opens what it is given by (name-stream), lib/files/names.inc, so the
	;objects of the store are shown too, store:path. It answers the once,
	;to the mailbox reply, the path taken or "" for none, and goes. The
	;picker's task, to be told "" if it is to go before it answers
	(defq picker (open-child "apps/system/files/child.lisp" +kn_call_pin))
	;each field something, (split) leaves out an empty one: * all kinds, - no app
	(mail-send picker (cat reply title "|" (ifn root ".") "|" (if (nempty? (ifn kinds "")) kinds "*")
		"|" (if save "save" "open") "|" (if (nempty? (ifn app "")) app "-") "|" (if names "names" "-")))
	picker)
