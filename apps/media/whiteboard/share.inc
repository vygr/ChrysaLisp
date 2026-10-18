;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a board shared between machines, lib/cwb/share.inc does the document
;
; A board that shares is the host, the teacher's, and says so as
; *Whiteboard, which every machine of the mesh sees. A board that joins
; asks it, is given where its ids and its users begin, puts its own
; document by, and has the host's. When the host leaves, the session ends
; and each has its own back. What goes between them is text: a letter for
; what it is, the mailbox it is from, a comma, and what it carries:
;	i	an invite, from the host, with its user's name
;	h	hello, I would join
;	w	welcome: ids from, users from, may a copy be saved, are they
;		teachers, each 1 or 0
;	d	a delta, (share-delta)
;	a	the last delta is had
;	s	may a copy be saved, from the host as it is changed, 1 or 0
;	t	are the boards that joined teachers, from the host, 1 or 0
;	b	bye
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/cwb/share.inc")

;:nil, :asking, :joined or :host. The host's *Whiteboard, the boards
;joined to it, and how many have, each given ids and users of its own.
;A board that joined: the host's mailbox, its own document put by, and
;whether a copy may be saved, which the host says and is no unless it does
(defq *share* :nil *share_entry* :nil *share_peers* (list) *share_joined* 0
	*share_host* :nil *share_own_doc* :nil *share_may_save* :nil
	;are the boards that joined teachers, who rub out what anyone made: a
	;class, no, its students rub out what is theirs; people who trust each
	;other working together, yes
	*share_trust* :nil
	;the host that invited this one, and its user's name
	*share_invite* :nil *share_invite_name* :nil)

(defun share-title ()
	;the title says if the board is shared, and how
	(def *title* :text (cat "Whiteboard" (case *share*
		(:host (cat ", shared" (if *share_may_save* ", copies may be saved" "") (if *share_trust* ", all teachers" "")))
		(:joined (cat ", joined" (if *share_may_save* ", may save a copy" "") (if *share_trust* ", a teacher" "")))
		(:asking ", joining")
		(:t (if *share_invite* (cat ", invited by " *share_invite_name*) "")))))
	(.-> *title* :layout :dirty_all))

(defun share-own ()
	; (share-own) -> hex
	(hex-encode (elem-get select +select_share)))

(defun share-send (to kind &optional payload)
	(mail-send (hex-decode to) (cat kind (share-own) "," (ifn payload ""))))

(defun share-forget ()
	(when *share_entry*
		(mail-forget *share_entry*)
		(setq *share_entry* :nil)))

(defun share-declare (host)
	;every board says it is there, to be invited, the host that it is shared
	(share-forget)
	(setq *share_entry* (mail-declare (elem-get select +select_share) "*Whiteboard"
		(cat (ifn (get :user *env*) "Guest") (if host "+host" "")))))

(defun share-boards ()
	; (share-boards) -> ((mailbox hex info) ...)
	;the other boards of the mesh
	(defq own (share-own))
	(reduce (lambda (out entry)
			(defq parts (split entry ","))
			(and (= (length parts) 4) (not (eql (elem-get parts 1) own))
				(push out (list (elem-get parts 1) (elem-get parts 3))))
			out)
		(mail-enquire "*Whiteboard") (list)))

(defun share-host-found ()
	; (share-host-found) -> :nil | mailbox hex
	;a board of the mesh that is shared, not this one
	;the one that invited this one first, if it still is
	(defq boards (share-boards))
	(or (and *share_invite* (some (lambda ((mbox info)) (and (eql mbox *share_invite*) (ends-with "+host" info) mbox)) boards))
		(some (lambda ((mbox info)) (and (ends-with "+host" info) mbox)) boards)))

(defun share-begin ()
	;join the board that is shared, or share this one if none is
	(cond
		((defq host (share-host-found))
			(setq *share* :asking *share_host* host)
			(share-send host "h")
			(share-title))
		(:t (setq *share* :host *share_peers* (list) *share_joined* 0)
			(share-start *board* :t)
			(share-declare :t)
			;the teacher invites: each other board is told, and joins if its
			;user says so, Chris: "the teacher/presenter is the one who
			;invites the colabs/students"
			(each (lambda ((mbox &ignore)) (share-send mbox "i" (ifn (get :user *env*) "Guest"))) (share-boards))
			(share-title))))

(defun share-kept-file ()
	; (share-kept-file) -> path
	;where a board that joins keeps its own document, unsaved as it may be,
	;in the user's own folder, not over the file it came from
	(cat (get :home *env*) "whiteboard_kept.cwb"))

(defun share-keep (doc)
	;the document of a board that joins, as it is, put by: "they may be in
	;the middle of doing something and not want to save at that point".
	;What it was saved as goes beside it, to be had again
	(cwb-save doc (file-stream (share-kept-file) +file_open_write))
	(save (ifn *file* "") (cat (share-kept-file) ".name")))

(defun share-kept-forget ()
	(pii-remove (share-kept-file))
	(pii-remove (cat (share-kept-file) ".name")))

(defun share-kept-back ()
	;a board closed, or a machine stopped, while it was joined has what it
	;put by back as it opens, under the name it had, and it is put by no more
	(when (and (pii-fstat (defq kept (share-kept-file))) (board-load kept))
		(defq was (ifn (load (cat kept ".name")) ""))
		(setq *file* (if (nempty? was) was :nil))
		(share-kept-forget)
		(def *title* :text "Whiteboard, brought back from before a share")
		(.-> *title* :layout :dirty_all)))

(defun share-end ()
	;a board that joined has its own document back, and it is put by no more
	(share-stop *board*)
	(share-kept-forget)
	(def *board* :user_base :nil :trusted :nil)
	(if *share_own_doc* (. *board* :set_doc *share_own_doc*))
	(setq *share* :nil *share_host* :nil *share_own_doc* :nil *share_may_save* :nil *share_trust* :nil)
	(share-title))

(defun share-switch-show (sym on)
	;a switch beside Share lit while it is on, as a toggle is
	(when (defq btn (some (# (if (eql (get :text %0) (num-to-utf8 sym)) %0)) (. *main_toolbar* :children)))
		(if on
			(def (. btn :dirty) :paper (canvas-brighter (get :paper *main_toolbar*)))
			(undef (. btn :dirty) :paper))))

(defun share-copies-show ()
	(share-switch-show +sym_copy *share_may_save*)
	(share-switch-show +sym_eraser *share_trust*))

(defun action-teachers ()
	;the host says if the boards joined to it are teachers, who rub out
	;what anyone made, and tells them now
	(when (eql *share* :host)
		(setq *share_trust* (not *share_trust*))
		(each (# (share-send %0 "t" (if *share_trust* "1" "0"))) *share_peers*)
		(share-copies-show)
		(share-title)))

(defun action-copies ()
	;the host says if the boards joined to it may save a copy of the
	;session, to their own files, and tells them now
	(when (eql *share* :host)
		(setq *share_may_save* (not *share_may_save*))
		(each (# (share-send %0 "s" (if *share_may_save* "1" "0"))) *share_peers*)
		(share-copies-show)
		(share-title)))

(defun share-leave ()
	;the host ends the session for all, one that joined leaves it
	(cond
		((eql *share* :host)
			(each (# (share-send %0 "b")) *share_peers*)
			(share-stop *board*)
			(share-declare :nil)
			(setq *share* :nil *share_peers* (list) *share_may_save* :nil *share_trust* :nil)
			(share-copies-show)
			(share-title))
		((find *share* '(:joined :asking))
			(share-send *share_host* "b")
			(share-end))))

(defun action-share ()
	(if *share* (share-leave) (share-begin)))

(defun share-message (msg)
	;what a board this is shared with sends
	(defq at (find "," msg) kind (first msg) from (slice msg 1 at) payload (slice msg (inc at) -1))
	(cond
		((eql kind "i")
			;invited: the title says so, and the share button joins it
			(unless *share*
				(setq *share_invite* from *share_invite_name* payload)
				(share-title)))
		((eql kind "h")
			;one would join: it is given ids and users of its own, from far
			;enough on that no two meet
			(when (eql *share* :host)
				(share-add *board* from)
				(push *share_peers* from)
				(defq n (++ *share_joined*))
				(share-send from "w" (cat (str (* n 1000000)) "," (str (* n 100)) "," (if *share_may_save* "1" "0")
					"," (if *share_trust* "1" "0")))))
		((eql kind "w")
			(when (eql *share* :asking)
				(setq *share_invite* :nil)
				(bind '(base users save &optional trust) (map (const str-to-num) (split payload ",")))
				(defq doc (. *board* :get_doc))
				(share-keep doc)
				(setq *share_own_doc* doc *share* :joined *share_may_save* (= save 1) *share_trust* (eql trust 1))
				(def *board* :trusted *share_trust*)
				(. *board* :set_doc (cwb-doc (. doc :find :width) (. doc :find :height)))
				(share-start *board*)
				(share-add *board* from)
				(share-id-base *board* base)
				(def *board* :user_base users)
				(share-title)))
		((eql kind "d")
			(when (find *share* '(:host :joined))
				(share-apply *board* payload from)
				(share-send from "a")))
		((eql kind "a")
			(if (find *share* '(:host :joined)) (share-acked *board* from)))
		((eql kind "s")
			;the host has said again if a copy may be saved
			(when (and (eql *share* :joined) (eql from *share_host*))
				(setq *share_may_save* (eql payload "1"))
				(share-title)))
		((eql kind "t")
			;the host has said again if this board is a teacher
			(when (and (eql *share* :joined) (eql from *share_host*))
				(setq *share_trust* (eql payload "1"))
				(def *board* :trusted *share_trust*)
				(share-title)))
		((eql kind "b")
			(cond
				((eql *share* :host)
					(share-drop *board* from)
					(setq *share_peers* (filter (# (not (eql %0 from))) *share_peers*)))
				((and (eql *share* :joined) (eql from *share_host*))
					(share-end))))))

(defun share-tick ()
	;each board this is shared with, that has had the last it was sent, is
	;sent what has changed since
	(when (find *share* '(:host :joined))
		(each (lambda (peer)
				(if (defq text (share-delta *board* peer)) (share-send peer "d" text)))
			(if (eql *share* :host) *share_peers* (list *share_host*)))))
