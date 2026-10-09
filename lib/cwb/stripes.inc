;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/stripes.inc - a board drawn in stripes by the nodes of the machine
;
; A document of very many shapes takes one task a long time to draw. Here
; the canvas it is drawn on has its pixels in shared memory, and each node
; of this machine has a child, lib/cwb/stripes_child.lisp, that draws a
; stripe of the rows straight onto them, as the Canvas demo does.
;
; A child has to have the document. It is not sent it each time. Each child
; keeps a copy, and what its shapes flatten to with it, and the document
; here has a version, a number that goes up each time it changes. A stripe
; to draw says which version. A child that has an older one asks, and is
; sent what has changed since the one it has: the layers, as the ids of
; their items in order, and each item that is new or not what it was. So a
; child flattens a shape once, and the flattening is done by all of them at
; once, each for the shapes it has not seen.
;
; What changed at each version is kept for the last so many. A child too
; far behind, or after a change that was not said item by item, is sent
; the whole document.
;
; Nothing here needs a screen, a test draws a board this way and looks at
; the pixels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")
(import "./board.inc")
(import "./paper.inc")

;a stripe for a child to draw: the key of the shared pixels and their size,
;the version of the document and where to ask for it, the rows, the zoom as
;a fixed in a whole number, and the paper. After it, text, the ids of the
;items not to draw, those being moved about, with a space between
(structure +stripe +job_size
	(long shared version zoom back)
	(netid sync)
	(uint width height y y1 gap style)
	(offset skip))

;its answer, the first row, and how many shapes it drew, -1 if it could
;not reach the pixels or the document
(structure +stripe_reply +job_reply_size
	(uint y)
	(int drawn))

;a child asking for the document: where to send it, and the version it has,
;-1 for none
(structure +stripe_sync 0
	(netid reply)
	(long have))

(defq +stripes_log 64 +stripe_styles ''(:plain :grid :lines :axis))

(defun stripes-full? (log have version)
	; (stripes-full? log have version) -> :t | :nil
	;does a copy at version have need the whole document to be at
	;version. log is ((version changes) ...), the newest last, changes is
	;ids or :all. It does if what happened since have is not all in the
	;log, or any of it was :all
	(defq since (filter (# (> (first %0) have)) log))
	(or (< have 0) (/= (length since) (- version have))
		(some (# (eql (second %0) :all)) since)))

(defun stripes-delta (doc log have version)
	; (stripes-delta doc log have version) -> text
	;what a copy of the document at version have needs to be the document
	;as it is, at version: the layers, as the ids of their items, and the
	;items that changed, or all of them
	(defq full (stripes-full? log have version)
		ids (if full :all (reduce (# (each (lambda (id) (unless (find id %0) (push %0 id))) (second %1)) %0)
			(filter (# (> (first %0) have)) log) (list)))
		items (list) stream (string-stream (cat "")))
	(each (lambda ((name flags layer_items))
		(each (# (if (or full (find (elem-get %0 +cwb_id) ids)) (push items (cwb-item-out %0)))) layer_items))
		(cwb-layers doc))
	(tree-save stream (scatter (Emap)
		:version version :full (if full 1 0)
		:width (. doc :find :width) :height (. doc :find :height)
		:layers (map (lambda ((name flags layer_items))
			(list name flags (map (# (elem-get %0 +cwb_id)) layer_items))) (cwb-layers doc))
		:items items))
	(str stream))

(defun stripes-apply (doc items text)
	; (stripes-apply doc items text) -> :nil | version
	;make a copy of a document what the text of a delta says it is. items
	;is the copy's items by id, an Fmap, those that are not sent again are
	;kept, with what they are flattened to. The version it is then at
	(when (and (defq data (catch (tree-load (string-stream text)) :t)) (Emap? data))
		(if (/= (. data :find :full) 0) (. items :empty))
		;a node has other tasks, they are given a turn every so many items
		(each (# (defq item (cwb-item-in %0)) (. items :insert (elem-get item +cwb_id) item)
				(if (= (logand (!) 63) 63) (task-slice)))
			(ifn (. data :find :items) (list)))
		(. doc :insert :width (. data :find :width))
		(. doc :insert :height (. data :find :height))
		(. doc :insert :layers (map (lambda ((name flags ids))
			(list name flags (reduce (# (if (defq item (. items :find %1)) (push %0 item)) %0) ids (list))))
			(ifn (. data :find :layers) (list))))
		(. data :find :version)))

(defclass Stripes (board &optional herd) :nil
	; (Stripes board [herd]) -> stripes
	;herd is the most children there are to be, default one for each node
	;of the machine, up to 64. They are kept off the node of the app, if
	;there is another: a child at work on a big document is a while
	;between the turns it gives the other tasks of its node
	;it has three mailboxes, (. stripes :mboxes), for the app to wait on
	;with its own, and what comes to one of them is given to (:handle).
	;
	;The children are idle, or being brought in step, :warming, or have a
	;:frame to draw. :sent is the version they were last sent word of,
	;and :synced the last they all answered that they have
	(def this :board board :herd (ifn herd 64) :jobs :nil :version 0 :log (list) :deltas (list)
		:task_mbox (mail-mbox) :reply_mbox (mail-mbox) :sync_mbox (mail-mbox)
		:state :idle :up :nil :sent 0 :synced -1 :drawn 0 :bad 0 :stamp (pii-time))

	(defmethod :mboxes ()
		; (. stripes :mboxes) -> (task_mbox reply_mbox sync_mbox)
		(list (get :task_mbox this) (get :reply_mbox this) (get :sync_mbox this)))

	(defmethod :note ()
		; (. stripes :note) -> version
		;the document has changed, by what the board says has. Called each
		;time it has, whoever then draws it
		(defq version (inc (get :version this)) log (get :log this))
		(push log (list version (. (get :board this) :take_changes)))
		(if (> (length log) +stripes_log) (setq log (slice log (- (length log) +stripes_log) -1)))
		(def this :version version :log log :deltas (list))
		version)

	(defmethod :start ()
		; (. stripes :start) -> stripes
		;start the children, on the nodes of this machine, and bring them
		;in step. Till they all answer (:ready?) is :nil
		(unless (get :jobs this)
			(def this :jobs (Jobs "lib/cwb/stripes_child.lisp" (get :task_mbox this) (get :reply_mbox this)
				(list (get :herd this)) :t) :up :nil :state :idle :synced -1)
			(. this :warm))
		this)

	(defmethod :warm ()
		; (. stripes :warm) -> :t | :nil
		;bring the children in step with the document as it is, each is
		;sent a stripe of no rows. :nil if they are not there or not idle.
		;When the last answers (:handle) says :warm
		(when (and (defq jobs (get :jobs this)) (eql (get :state this) :idle))
			(def this :state :warming :sent (get :version this) :bad 0 :stamp (pii-time))
			(. jobs :add (map (lambda (&) (. this :job :nil 0 0 0 0 1.0 0 :plain 32 (list)))
				(range 0 (max 1 (. jobs :size)))))
			:t))

	(defmethod :ready? ()
		; (. stripes :ready?) -> :t | :nil
		;are the children up, and idle
		(and (get :up this) (eql (get :state this) :idle)))

	(defmethod :busy? ()
		; (. stripes :busy?) -> :t | :nil
		;is a frame out with the children
		(eql (get :state this) :frame))

	(defmethod :cheap? ()
		; (. stripes :cheap?) -> :t | :nil
		;are the children ready, and near enough in step that a frame
		;would not have each sent the whole document first
		(and (. this :ready?)
			(not (stripes-full? (get :log this) (get :synced this) (get :version this)))))

	(defmethod :job (key width height y y1 zoom back style gap skip)
		; (. stripes :job key width height y y1 zoom back style gap skip) -> job
		(cat (setf-> (str-alloc +stripe_size)
				(+stripe_shared (ifn key 0)) (+stripe_version (get :version this))
				(+stripe_zoom (n2i (* (n2f zoom) 65536.0))) (+stripe_back back)
				(+stripe_sync (get :sync_mbox this))
				(+stripe_width width) (+stripe_height height) (+stripe_y y) (+stripe_y1 y1)
				(+stripe_gap gap) (+stripe_style (ifn (find style +stripe_styles) 0)))
			(join (map (const str) skip) " ")))

	(defmethod :frame (canvas zoom style)
		; (. stripes :frame canvas zoom style) -> :t | :nil
		;draw the document, all of it, onto a canvas whose pixels are
		;shared, a stripe for each child. :nil, and nothing is done, if
		;the children are not ready or the canvas is not shared. When the
		;last stripe is answered (:handle) says :done
		(defq key (canvas-key canvas) board (get :board this) doc (. board :get_doc))
		(when (and (/= key 0) (. this :ready?))
			(bind '(w h) (. canvas :pref_size))
			(defq jobs (get :jobs this) count (max 1 (. jobs :size)))
			(def this :state :frame :sent (get :version this) :drawn 0 :bad 0 :stamp (pii-time))
			(. jobs :add (map (# (. this :job key w h (/ (* %0 h) count) (/ (* (inc %0) h) count)
					zoom (. doc :find :background) style
					(max 2 (n2i (* (n2f (. doc :find :grid)) (n2f zoom)))) (get :floating board)))
				(range 0 count)))
			:t))

	(defmethod :handle (index msg)
		; (. stripes :handle index msg) -> :nil | :warm | :done | :failed
		;what came to one of the three mailboxes, index says which, 0 1 or
		;2. :warm when the last child has answered that it is in step,
		;:done when it was the last stripe of a frame, :failed if it was
		;and a child could not draw its stripe, the canvas is then not all
		;drawn
		(defq jobs (get :jobs this) result :nil)
		(case index
			(0 (if jobs (. jobs :launched msg)))
			(1 (when (and jobs (defq out (. jobs :answered msg)))
					(defq drawn (getf msg +stripe_reply_drawn))
					(def this :drawn (+ (get :drawn this) (max 0 drawn))
						:bad (+ (get :bad this) (if (< drawn 0) 1 0)))
					(when (= out 0)
						(defq good (= (get :bad this) 0))
						(setq result (if (eql (get :state this) :frame) (if good :done :failed) :warm))
						(if good (def this :up :t :synced (get :sent this)))
						(def this :state :idle :stamp (pii-time)))))
			(2 ;a child that is behind. What one that far behind needs is
				;made once, the next to ask from there is sent the same
				(defq have (getf msg +stripe_sync_have) deltas (get :deltas this)
					text (if (defq at (find have (map (const first) deltas)))
						(second (elem-get deltas at))
						(second (last (push deltas (list have
							(stripes-delta (. (get :board this) :get_doc) (get :log this)
								have (get :version this))))))))
				(mail-send (getf msg +stripe_sync_reply) text)))
		result)

	(defmethod :keep ()
		; (. stripes :keep) -> stripes
		;called now and then, once a second say. A child that has gone, or
		;been too long over a stripe, is started again. Children that have
		;had nothing to do for a while are brought in step, which is also
		;what tells them not to go
		(when (defq jobs (get :jobs this))
			(. jobs :refresh (task-timeout 5))
			(if (and (eql (get :state this) :idle) (> (- (pii-time) (get :stamp this)) 5000000))
				(. this :warm)))
		this)

	(defmethod :close ()
		; (. stripes :close) -> stripes
		(when (get :jobs this)
			(. (get :jobs this) :close)
			(def this :jobs :nil :up :nil :state :idle :synced -1))
		this)
	)
