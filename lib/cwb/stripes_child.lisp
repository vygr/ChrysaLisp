;a child of lib/cwb/stripes.inc. It draws stripes of a board onto the
;pixels of the app's canvas, in shared memory, from its own copy of the
;document, which it asks for when what it has is not the version a stripe
;is of. It goes when it is told to, or has had nothing to do for a while.
;
;It does not look at every item for every stripe. For the version it has
;it knows, for each band of rows of the document, +band rows deep, which
;items of each layer have any part in it, in their order. A stripe is the
;items of the bands it lies on.
(import "gui/lisp.inc")
(import "lib/cwb/stripes.inc")

(enums +select 0
	(enum main timeout))

(defq shared_key 0 shared_size :nil canvas :nil
	doc (cwb-doc) items (Fmap 101) have -1 sync_mbox (mail-mbox)
	bands :nil +band 64.0)

(defun band-of (y count)
	;the band a row of the document is in, the first or the last if it
	;is off the document
	(max 0 (min (dec count) (n2i (floor (/ y +band))))))

(defun make-bands ()
	;for each layer (flags bands none), bands a list, for each band the
	;(place item) of each item with any part in it, place its place in
	;the layer, and none a :nil for each item of the layer. An item that
	;draws nothing is in no band
	(defq count (inc (n2i (/ (n2f (. doc :find :height)) +band))))
	(map (lambda ((name flags layer_items))
		(defq rows (map (lambda (&) (list)) (range 0 count)))
		(each (lambda (item)
			;every shape is flattened here, the first time, which is a
			;while. The other tasks of the node are given a turn
			(if (= (logand (!) 31) 31) (task-slice))
			(when (defq box (cwb-bounds (list item)))
				(defq entry (list (!) item))
				;a row more each way, as the stripe has
				(each (# (push (elem-get rows %0) entry))
					(range (band-of (- (second box) 1.0) count) (inc (band-of (+ (elem-get box 3) 1.0) count))))))
			layer_items)
		(list flags rows (map (lambda (&) :nil) layer_items)))
		(cwb-layers doc)))

(defun band-items (rows none y y1)
	;the items of a layer with any part in rows y to y1 of the document,
	;in their order
	(defq count (length rows) b0 (band-of y count) b1 (band-of y1 count))
	(cond
		((= b0 b1) (map (const second) (elem-get rows b0)))
		(:t ;each is put at its place among the nones, one that is in
			;two of the bands at the same place twice, and the nones
			;are left out
			(defq places (cat none))
			(each (lambda (band) (each (lambda ((place item)) (elem-set places place item)) band))
				(slice rows b0 (inc b1)))
			(filter (const identity) places))))

(defun attach (key width height)
	;the app's canvas, found again if it is another, or another size. :nil
	;if this node can not reach the pixels
	(unless (and (= key shared_key) (eql shared_size (list width height)))
		(setq shared_key key shared_size (list width height)
			canvas (and (/= key 0)
				(defq found (canvas-shared width height 1 key))
				(. found :set_canvas_flags +canvas_flag_antialias))))
	canvas)

(defun in-step (version sync)
	;have the document at that version, or a newer, asking for it if not.
	;:nil if no answer comes
	(or (>= have version)
		(progn
			(mail-send sync (setf-> (str-alloc +stripe_sync_size)
				(+stripe_sync_reply sync_mbox) (+stripe_sync_have have)))
			(and (defq text (mail-read-timeout sync_mbox (task-timeout 10)))
				(defq now (stripes-apply doc items text))
				(setq bands :nil have now)
				(>= have version)))))

(defun draw-stripe (key reply msg)
	(bind '(shared version zoom back sync width height y y1 gap style)
		(getf-> msg +stripe_shared +stripe_version +stripe_zoom +stripe_back +stripe_sync
			+stripe_width +stripe_height +stripe_y +stripe_y1 +stripe_gap +stripe_style))
	(defq zoom (/ (n2f zoom) 65536.0)
		skip (map (const str-as-num) (split (slice msg +stripe_skip -1) " "))
		drawn (cond
			;no rows at all is the app asking this child to be in step
			((>= y y1)
				(cond
					((in-step version sync) (unless bands (setq bands (make-bands))) 0)
					(:t -1)))
			((not (attach shared width height)) -1)
			((not (in-step version sync)) -1)
			(:t (. canvas :set_clip 0 y width y1)
				(cwb-paper canvas width height back (elem-get +stripe_styles style) gap y y1)
				(unless bands (setq bands (make-bands)))
				;a row more each way, an edge that lies on the line between
				;two rows can shade the one it is not in
				(defq m (if (= zoom 1.0) :nil (cwb-mat-scale zoom))
					clip (list 0.0 (n2f (dec y)) (n2f width) (n2f (inc y1))))
				(reduce (lambda (count (flags rows none))
						(if (bits? flags 1) count
							(progn
								(defq found (band-items rows none (/ (second clip) zoom) (/ (elem-get clip 3) zoom)))
								(+ count (cwb-draw-items canvas
									(if (nempty? skip)
										(filter (# (not (find (elem-get %0 +cwb_id) skip))) found)
										found)
									m clip)))))
					bands 0))))
	(mail-send reply (setf-> (str-alloc +stripe_reply_size)
		(+job_reply_key key) (+stripe_reply_y y) (+stripe_reply_drawn drawn))))

(defun main ()
	(defq select (task-mboxes +select_size) running :t +timeout 20000000)
	(while running
		(mail-timeout (elem-get select +select_timeout) +timeout 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				(setq running :nil))
			((= idx +select_main)
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(draw-stripe (getf msg +job_key) (getf msg +job_reply) msg)))))
