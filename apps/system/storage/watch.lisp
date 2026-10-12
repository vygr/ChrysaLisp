;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, what a store is now, looked at for the app
;
; A task of its own, started by the Storage app. A store that is busy is
; slow to say how it is, and what asks it waits. So the app does not ask:
; this does, over and over, and mails the app what there is to draw, and
; the app draws what it was last sent and is never kept from its window.
;
; It is mailed who to tell, then, as it goes: "group " the group the
; members are known by and 1 if they are the app's own, a demo; "show "
; and a path, that object is the one to say the blocks of; "stop".
;
; It sends two things, each a list, printed. When what objects there are
; changes, ("list" rows), every object, in order, each (path bytes writes
; writer who), who a number for each member, 0 it has no such path, 1 it
; has, 2 it has another of that name. The app has the whole list, so it
; goes up and down it with nothing asked of anyone. And at each look,
; ("look" members total has entry kept): the members that are there, each
; (id where blocks cut); how many blocks the object shown has, with its
; manifest; for each member which of the first of them it keeps, a str of
; 1 and 0; the object that is of, (path bytes); and the fewest and the
; most members that any one block of it is kept by.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/storage/app.inc")

;how often it looks, at the most, and how many blocks of an object the
;app has room to draw
(defq +rate 500000 +columns 115
	;how long, in all, the members are waited for at a look
	+patience 1500000)

(defun name-key (name)
	;a name and the number it ends with, so that 2 is before 10
	(defq at (length name))
	(while (and (> at 0) (<= (ascii-code "0") (code name 1 (dec at)) (ascii-code "9"))) (setq at (dec at)))
	(list (slice name 0 at) (if (< at (length name)) (str-as-num (slice name at -1)) -1)))

(defun look ()
	; (look) -> (members objects names)
	;the members that are there, each (id netid blocks cut digest said), in
	;order; the objects, every one that any of them has; and the blocks of
	;the one shown. *who* is, for each object, what each member has of it:
	;:nil, :t, or :differs, it has the path and it is not the same
	(defq there (list))
	(each (lambda (entry)
		(defq parts (split entry ",") about (split (last parts) " "))
		(if (= (length about) 2) (push there (list (second about) (hex-decode (second parts)) (first about)))))
		(mail-enquire (cat *group* ",")))
	;a store that is not a demo: only the members of this system's own
	(unless *demo*
		(and (defq mine (storage-service)) (defq info (storage-info mine))
			(setq there (filter (# (eql (third %0) (. info :find :id))) there))))
	(sort there (# (cmp (first %0) (first %1))))
	;each is asked how it is, all of them at once, and not waited for
	;past a time: one that is busy is slow to say, and is then as it last
	;said it was
	(defq asked (map (lambda ((id where &ignore))
			(list (storage-send where +storage_type_info) (storage-send where +storage_type_state))) there)
		by (+ (pii-time) +patience)
		members (map (lambda ((id where &ignore) (info_box state_box))
			(defq info (storage-info-read (mail-read-timeout info_box (max 1000 (- by (pii-time)))))
				state (storage-state-read (mail-read-timeout state_box (max 1000 (- by (pii-time)))))
				was (ifn (. *last* :find id) (list 0 (list) "")))
			(. *last* :insert id (defq is (list (if info (. info :find :blocks) (first was))
				(if state (. state :find :cut) (second was)) (if state (. state :find :digest) (third was)))))
			;and whether it said anything: one that did not is asked no more
			;at this look. Members that have gone, a demo that was stopped,
			;were waited for one after another, a quarter of a minute of it
			(cat (list id where) is (list (if (or info state) :t))))
		there asked)
		objects *objects* names (list) changed :nil now (pii-time))
	;what each has, asked again only of one that is not as it was, and
	;when there is a lot of it not at every look: (digest entries paths)
	(when (or (< (length *objects*) 300) (> (- now *listed*) 2000000))
		(setq *listed* now)
		(each (lambda ((id where blocks cut digest said))
			(unless (or (not said) (and (defq had (. *seen* :find id)) (eql (first had) digest)))
				;one that does not say in the time is as it was, and is asked again
				(when (defq answer (storage-ask where +storage_type_list "" 3))
					(defq entries (map (const store-entry-read) (filter (const nempty?) (split answer +storage_lf)))
						paths (Fmap 1021))
					(each (# (. paths :insert (first %0) (elem-get %0 +store_entry_hash))) entries)
					(. *seen* :insert id (list digest entries paths))
					(setq changed :t))))
			members)
		(unless (= (length members) *members*) (setq changed :t *members* (length members))))
	(when changed
		(defq known (Fmap 1021) keyed (list))
		(each (lambda ((id &ignore))
			(each (lambda (entry)
				(unless (. known :find (first entry))
					(. known :insert (first entry) :t)
					(push keyed (list (name-key (first entry)) entry))))
				(second (ifn (. *seen* :find id) (list "" (list) (Fmap 1))))))
			members)
		(sort keyed (lambda ((one &ignore) (two &ignore))
			(if (eql (first one) (first two)) (- (second one) (second two)) (cmp (first one) (first two)))))
		(setq objects (map (const second) keyed) *objects* objects
			*who* (map (lambda (entry)
				(map (lambda ((id &ignore))
					(if (defq theirs (and (defq had (. *seen* :find id)) (. (third had) :find (first entry))))
						(if (eql theirs (elem-get entry +store_entry_hash)) :t :differs)))
					members))
				objects)))
	(when changed
		(setq *changed* :t *index* (Fmap 1021))
		(each (# (. *index* :insert (first %0) %0)) objects))
	(setq *entry* (if (nempty? objects) (ifn (and *path* (. *index* :find *path*)) (first objects))))
	(when *entry*
		(defq entry *entry* hash (elem-get entry +store_entry_hash))
		;its manifest, from whichever member has it, then its blocks. What
		;an object is made of does not change, so the last one is kept
		(cond
			((eql hash (first *made_of*)) (setq names (second *made_of*)))
			((defq text (some (lambda ((id where blocks cut digest said))
					(and said (defq data (storage-ask where +storage_type_block_get hash 2)) (eql (sha256 data) hash) data)) members))
				(defq lines (filter (const nempty?) (split text (ascii-char 10))))
				(setq names (cat (list hash) (map (const hex-decode) (slice lines 2 -1))) *made_of* (list hash names)))))
	(list members objects names))

(defun rows ()
	;every object, as the app lists them
	(map (lambda (entry who)
		(list (first entry) (elem-get entry +store_entry_bytes) (store-vv-total (elem-get entry +store_entry_vv))
			(elem-get entry +store_entry_writer)
			(map (# (cond ((eql %0 :differs) 2) (%0 1) (0))) who)))
		*objects* *who*))

(defun snapshot ()
	;what there is to say of the members and of the object shown. Which
	;of all its blocks each member keeps, and from that the fewest and the
	;most members any one block is kept by: one kept by none is a hole in
	;the object, and is to be seen
	(bind '(members objects names) (look))
	(defq total (length names) body (apply (const cat) (cat (list "") names))
		asked (map (lambda ((id where blocks cut digest said))
			(and said (nempty? names) (storage-send where +storage_type_has body))) members)
		by (+ (pii-time) +patience)
		has (map (# (if %0 (ifn (mail-read-timeout %0 (max 1000 (- by (pii-time)))) "") "")) asked)
		fewest (length members) most 0)
	(each (lambda (col)
		(defq keepers (length (filter (# (and (< col (length %0)) (eql (elem-get %0 col) "1"))) has)))
		(setq fewest (min fewest keepers) most (max most keepers)))
		(range 1 total))
	(list "look"
		(map (lambda ((id where blocks cut &ignore)) (list id (hex-encode where) blocks cut)) members)
		total (map (# (slice %0 0 (min (length %0) +columns))) has)
		(if *entry* (list (first *entry*) (elem-get *entry* +store_entry_bytes)) (list))
		(if (> total 1) (list fewest most) (list))))

(defun main ()
	(defq reply (hex-decode (mail-read (task-mbox))) going :t
		*group* +storage_group *demo* :nil *path* :nil *entry* :nil *changed* :nil *index* (Fmap 11)
		*seen* (Fmap 11) *listed* 0 *objects* (list) *who* (list) *members* 0 *made_of* (list :nil (list))
		*last* (Fmap 11))
	(while going
		(defq t0 (pii-time) snap (catch (snapshot) :t))
		;the list first, if it is not as it was, then the look
		(when *changed*
			(setq *changed* :nil)
			(mail-send reply (str (list "list" (rows)))))
		(if (list? snap) (mail-send reply (str snap)))
		;what is said to it, or the time to look again, no sooner than as
		;long again as the look took. Of several said, the last is what counts
		(defq msg (mail-read-timeout (task-mbox) (max +rate (- (pii-time) t0))))
		(while msg
			(cond
				((eql msg "stop") (setq going :nil))
				((starts-with "show " msg) (setq *path* (slice msg 5 -1)))
				((starts-with "group " msg)
					(bind '(group demo) (split (slice msg 6 -1) " "))
					(setq *group* group *demo* (eql demo "1") *path* :nil *entry* :nil *changed* :t *index* (Fmap 11)
						*seen* (Fmap 11) *listed* 0 *objects* (list) *who* (list) *members* 0)))
			(setq msg (if (mail-poll (list (task-mbox))) (mail-read (task-mbox)))))))
