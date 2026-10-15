;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a board shared between machines, lib/cwb/share.inc does the document
;
; A board that shares is the host, the teacher's, and says so as
; *Whiteboard, which every machine of the mesh sees. A board that joins
; asks it, is given where its ids and its users begin, puts its own
; document by, and has the host's. When the host leaves, the session ends
; and each has its own back. What goes between them is text: a letter for
; what it is, the mailbox it is from, a comma, and what it carries:
;	h	hello, I would join
;	w	welcome: ids from, users from, may a copy be saved, 1 or 0
;	d	a delta, (share-delta)
;	a	the last delta is had
;	b	bye
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/cwb/share.inc")

;:nil, :asking, :joined or :host. The host's *Whiteboard, the boards
;joined to it, and how many have, each given ids and users of its own.
;A board that joined: the host's mailbox, its own document put by, and
;whether a copy may be saved, which the host says and is no unless it does
(defq *share* :nil *share_entry* :nil *share_peers* (list) *share_joined* 0
	*share_host* :nil *share_own_doc* :nil *share_may_save* :nil)

(defun share-title ()
	;the title says if the board is shared, and how
	(def *title* :text (cat "Whiteboard" (case *share* (:host ", shared") (:joined ", joined") (:asking ", joining") (:t ""))))
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

(defun share-host-found ()
	; (share-host-found) -> :nil | mailbox hex
	;a board of the mesh that is shared, not this one
	(defq own (share-own))
	(some (lambda (entry)
			(defq parts (split entry ","))
			(and (= (length parts) 4) (ends-with "+host" (elem-get parts 3))
				(not (eql (elem-get parts 1) own)) (elem-get parts 1)))
		(mail-enquire "*Whiteboard")))

(defun share-begin ()
	;join the board that is shared, or share this one if none is
	(cond
		((defq host (share-host-found))
			(setq *share* :asking *share_host* host)
			(share-send host "h")
			(share-title))
		(:t (setq *share* :host *share_peers* (list) *share_joined* 0)
			(share-start *board* :t)
			(setq *share_entry* (mail-declare (elem-get select +select_share) "*Whiteboard"
				(cat (ifn *env_user* "Guest") "+host")))
			(share-title))))

(defun share-end ()
	;a board that joined has its own document back
	(share-stop *board*)
	(def *board* :user_base :nil)
	(if *share_own_doc* (. *board* :set_doc *share_own_doc*))
	(setq *share* :nil *share_host* :nil *share_own_doc* :nil *share_may_save* :nil)
	(share-title))

(defun share-leave ()
	;the host ends the session for all, one that joined leaves it
	(cond
		((eql *share* :host)
			(each (# (share-send %0 "b")) *share_peers*)
			(share-stop *board*)
			(share-forget)
			(setq *share* :nil *share_peers* (list))
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
		((eql kind "h")
			;one would join: it is given ids and users of its own, from far
			;enough on that no two meet
			(when (eql *share* :host)
				(share-add *board* from)
				(push *share_peers* from)
				(defq n (++ *share_joined*))
				(share-send from "w" (cat (str (* n 1000000)) "," (str (* n 100)) "," (if *share_may_save* "1" "0")))))
		((eql kind "w")
			(when (eql *share* :asking)
				(bind '(base users save) (map (const str-to-num) (split payload ",")))
				(defq doc (. *board* :get_doc))
				(setq *share_own_doc* doc *share* :joined *share_may_save* (= save 1))
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
