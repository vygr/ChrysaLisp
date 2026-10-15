;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/share.inc - a board shared with boards on other machines
;
; Chris: "each side is sharing the canvas and document, but not the same
; modes etc. Same 'rudeness' rules apply with regards to lock outs." One
; board is the teacher's, who began the session, and each other board is
; joined to it alone: the teacher's passes on what each sends to the rest.
;
; What goes between them is not the document, "have to be document deltas,
; can't ship the whole thing every time", nor each step, "we don't need to
; update the remotes as fast as the board the person is using". A board
; keeps, for each board it sends to, what that one has: the ids of the
; document's items in order, and which items have changed since. When that
; one is not waiting on the last it was sent, it is sent the difference:
; the items that are new or changed, whole, those gone, and the order of
; all. So it is what is, not what was done: sent again it does nothing, a
; slow link is sent less and later and loses nothing, and a fast one is
; sent each frame. What is being drawn, and what a hand holds and moves,
; go with it, as they are then.
;
; Nothing here sends anything, it makes and takes text. The app sends it.
; A test can have boards share with no machine at all.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./board.inc")

;what a board keeps of each it sends to, by a key the app gives it
(enums +peer 0
	(enum last changed busy temps))

(defun share-ids (doc)
	; (share-ids doc) -> ids
	;the ids of the document's items, the back one first
	(map (# (elem-get %0 +cwb_id)) (cwb-items doc)))

(defun share-start (board)
	; (share-start board) -> board
	;from now what changes in the document is noted for those it is shared with
	(def board :share_log (list) :share_peers (Fmap 11) :remote_temp (Fmap 11))
	board)

(defun share-stop (board)
	; (share-stop board) -> board
	(def board :share_log :nil :share_peers :nil)
	(. (get :remote_temp board) :empty)
	(. board :touch +board_dirty_overlay))

(defun share-add (board key)
	; (share-add board key) -> board
	;a board to send to, that has nothing yet: the first it is sent is all
	(. (get :share_peers board) :insert key (list (list) (list :all) :nil :nil))
	board)

(defun share-drop (board key)
	; (share-drop board key) -> board
	;one no longer sent to, and what it was drawing goes
	(. (get :share_peers board) :erase key)
	(defq temps (get :remote_temp board) gone (list))
	(. temps :each (lambda (k (from shape)) (if (eql from key) (push gone k))))
	(each (# (. temps :erase %0)) gone)
	(. board :touch +board_dirty_overlay))

(defun share-tops (doc ids)
	; (share-tops doc ids) -> ids
	;the items of the document, not those in a group, that are or have
	;in them one of these
	(defq want (cwb-id-set ids) out (list))
	(each (lambda (item)
			(defq todo (list item) hit :nil)
			(while (and (not hit) (defq next (pop todo)))
				(if (cwb-id? want (elem-get next +cwb_id))
					(setq hit :t)
					(if (cwb-group? next) (each (# (push todo %0)) (elem-get next +cwb_items)))))
			(if hit (push out (elem-get item +cwb_id))))
		(cwb-items doc))
	out)

(defun share-note (board)
	; (share-note board) -> board
	;what has changed since this was last asked is told to each it sends to
	(defq log (get :share_log board))
	(when (nempty? log)
		(def board :share_log (list))
		(. (get :share_peers board) :each (lambda (key peer)
			(defq changed (elem-get peer +peer_changed))
			(cond
				((find :all changed))
				((find :all log) (elem-set peer +peer_changed (list :all)))
				(:t (each (# (unless (find %0 changed) (push changed %0))) log))))))
	board)

(defun share-temps (board key)
	; (share-temps board key) -> ((name shape) ...)
	;what is being drawn here, and on the boards that send here, but for
	;the one it is to go to: it has its own
	(defq out (list))
	(. (get :temp board) :each (lambda (id shape) (push out (list (str id) (cwb-item-out shape)))))
	(. (get :remote_temp board) :each (lambda (name (from shape))
		(unless (eql from key) (push out (list name (cwb-item-out shape))))))
	out)

(defun share-delta (board key)
	; (share-delta board key) -> :nil | text
	;what that one is to be sent now, if it is not waiting on the last, and
	;there is anything to send. It is then waiting, (share-acked)
	(share-note board)
	(when (and (defq peer (. (get :share_peers board) :find key)) (not (elem-get peer +peer_busy)))
		(bind '(last changed & sent_temps) peer)
		(defq doc (get :doc board) now (share-ids doc) gone (filter (# (not (find %0 now))) last)
			all (find :all changed)
			;what a hand holds moves with no change said, it goes each time
			held (share-tops doc (get :floating board))
			put (if all now (cat (share-tops doc changed) (filter (# (not (find %0 last))) now) held))
			temps (share-temps board key))
		(setq put (reduce (# (if (find %1 %0) %0 (push %0 %1))) put (list)))
		(when (or (nempty? put) (nempty? gone) (not (eql (str now) (str last))) (nempty? temps) sent_temps)
			(defq want (cwb-id-set put) out (string-stream (cat "")))
			(tree-save out (scatter (Emap)
				:ids now :gone gone :all (if all 1 0)
				:put (map (const cwb-item-out) (filter (# (cwb-id? want (elem-get %0 +cwb_id))) (cwb-items doc)))
				:temps temps
				:width (. doc :find :width) :height (. doc :find :height)))
			(elem-set peer +peer_last now)
			(elem-set peer +peer_changed (list))
			(elem-set peer +peer_busy :t)
			(elem-set peer +peer_temps (nempty? temps))
			(str out))))

(defun share-acked (board key)
	; (share-acked board key) -> board
	;that one has what it was last sent, the next can go
	(if (defq peer (. (get :share_peers board) :find key)) (elem-set peer +peer_busy :nil))
	board)

(defun share-apply (board text from)
	; (share-apply board text from) -> :t | :nil
	;make the document what a delta from that one says: the items it puts,
	;in the place of those of their ids or on top, those it says are gone
	;taken out, and those it knows in its order, in the places they are
	;in. What this board has that it does not know stays where it is. What
	;that one was drawing is drawn here. What changed is noted for the
	;rest, but for that one: it has it. Was it a delta
	(when (and (defq data (catch (tree-load (string-stream text)) :t)) (Emap? data))
		(defq doc (get :doc board) items (cwb-items doc)
			ids (ifn (. data :find :ids) (list)) gone (cwb-id-set (ifn (. data :find :gone) (list)))
			put (map (const cwb-item-in) (ifn (. data :find :put) (list))))
		;what is gone
		(defq kept (filter (# (not (cwb-id? gone (elem-get %0 +cwb_id)))) items))
		;what is put, in its own place, or on top
		(each (lambda (item)
				(defq id (elem-get item +cwb_id) at (some (# (if (= (elem-get %0 +cwb_id) id) (!))) kept))
				(if at (elem-set kept at item) (push kept item)))
			put)
		;those it knows, in its order, in the places they are in here
		(defq order (cwb-id-set ids)
			places (filter (# (cwb-id? order (elem-get (elem-get kept %0) +cwb_id))) (range 0 (length kept)))
			theirs (map (# (elem-get kept %0)) places))
		(sort theirs (# (- (find (elem-get %0 +cwb_id) ids) (find (elem-get %1 +cwb_id) ids))))
		(each (# (elem-set kept %0 %1)) places theirs)
		(clear items)
		(each (# (push items %0)) kept)
		(and (num? (defq w (. data :find :width))) (. doc :insert :width w))
		(and (num? (defq h (. data :find :height))) (. doc :insert :height h))
		;what it was drawing
		(defq temps (get :remote_temp board) old (list))
		(. temps :each (lambda (k (src shape)) (if (eql src from) (push old k))))
		(each (# (. temps :erase %0)) old)
		(each (lambda ((name shape)) (. temps :insert (cat (str from) "/" name) (list from (cwb-item-in shape))))
			(ifn (. data :find :temps) (list)))
		;noted for the rest, and that one has it all now
		(. board :changed (if (/= (. data :find :all) 0) :all (cat (map (# (elem-get %0 +cwb_id)) put) (map (const identity) (ifn (. data :find :gone) (list))))))
		(share-note board)
		;that one has what it said it has, the ids it sent, and the things it
		;put are not to go back to it
		(when (defq peer (. (get :share_peers board) :find from))
			(elem-set peer +peer_last ids)
			(defq mine (cwb-id-set (map (# (elem-get %0 +cwb_id)) put)))
			(elem-set peer +peer_changed (if (/= (. data :find :all) 0) (list)
				(filter (# (not (cwb-id? mine %0))) (elem-get peer +peer_changed)))))
		(. board :touch (+ +board_dirty_doc +board_dirty_overlay))
		:t))

(defun share-id-base (board base)
	; (share-id-base board base) -> board
	;a board that has joined gives new things ids from base on, so no two
	;boards give the same id to two things
	(defq doc (get :doc board))
	(if (< (. doc :find :next_id) base) (. doc :insert :next_id base))
	board)
