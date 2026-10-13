;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, what is asked of the service, and the asking
;
; The service, service/storage/app_impl.lisp, keeps a store,
; lib/storage/store.inc, and does what it is asked by mail. A request is a
; header, who to answer and what is asked, and then what that takes, text
; or the bytes of a block. An answer is one mail.
;
; An object goes to it a block at a time, and comes from it so: a member
; of a store asks another for blocks the same way an app does, and a mail
; is never longer than a block and a little.
;
; This file is all an app needs: (storage-put) (storage-get) and the rest.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/storage/store.inc")
(import "lib/streams/lisp_stream.inc")

(enums +storage_type 0
	(enum info stat list has block_put block_get commit delete
		pin pins_missing sweep check walk stop
		;what members ask of each other, and a tender of its own service
		state entries merge share block_drop where peers members cut
		copies pinned))

(structure +storage_rpc 0
	(netid reply_id)
	(uint type))

(defq +storage_name "@Storage" +storage_group "*Storage" +storage_wait 10 +storage_lf (ascii-char 10)
	;how often a tender looks, and how long a member is not heard from
	;before what it kept is kept by others, microseconds
	+storage_tick 2000000 +storage_grace 300000000
	;how long a block is, in hex, as a name is sent
	+storage_hex 64)

(defun storage-service (&optional name)
	; (storage-service [name]) -> :nil | netid
	;the Storage service of this system, or one of another name, a test's
	(and (nempty? (defq found (mail-enquire (cat (ifn name +storage_name) ","))))
		(hex-decode (second (split (first found) ",")))))

(defun storage-ask (svc type &optional body wait)
	; (storage-ask svc type [body wait]) -> :nil | answer
	;one thing asked of a service, and what it says. :nil if it says
	;nothing in the time, seconds. A new mailbox each time: an answer that
	;comes late is to a mailbox nobody reads
	(when svc
		(defq mbox (mail-mbox))
		(mail-send svc (cat (setf-> (str-alloc +storage_rpc_size)
			(+storage_rpc_reply_id mbox) (+storage_rpc_type type)) (ifn body "")))
		(mail-read-timeout mbox (task-timeout (ifn wait +storage_wait)))))

(defun storage-send (svc type &optional body)
	; (storage-send svc type [body]) -> mbox
	;one thing asked of a service and not waited for. Where what it says
	;will be, to be read when it is wanted: several can be asked at once,
	;of several services, and be on their way together
	(defq mbox (mail-mbox))
	(mail-send svc (cat (setf-> (str-alloc +storage_rpc_size)
		(+storage_rpc_reply_id mbox) (+storage_rpc_type type)) (ifn body "")))
	mbox)

(defun storage-result (answer)
	; (storage-result answer) -> (:ok entry) | (:stale entry) | (:missing names) | (:full :nil) | (:none :nil) | (:lost :nil)
	;what the service said of a write, as (store-commit) gives it. :lost
	;if it said nothing
	(defq lines (split (ifn answer "lost") +storage_lf) what (first lines))
	(cond
		((find what '("ok" "stale"))
			(list (if (eql what "ok") :ok :stale) (if (> (length lines) 1) (store-entry-read (second lines)))))
		((eql what "missing") (list :missing (map (const hex-decode) (rest lines))))
		((eql what "full") (list :full :nil))
		((eql what "none") (list :none :nil))
		((list :lost :nil))))

(defun storage-from (from)
	;what a path is said to be now, as it is sent: ? not said, - none
	(cond ((not from) "?") ((eql from "") "-") ((hex-encode from))))

(defun storage-info-read (answer)
	; (storage-info-read answer) -> :nil | emap
	;what a service said of its store, as (storage-info) gives it: for
	;what asked with (storage-send), of several at once
	(when answer
		(defq info (Emap))
		(each (lambda (line)
			(when (defq at (find " " line))
				(defq key (sym (cat ":" (slice line 0 at))) val (slice line (inc at) -1))
				(. info :insert key (cond
					((eql key :members) (split val " "))
					((find key '(:id :member)) val)
					((str-as-num val))))))
			(split answer +storage_lf))
		info))

(defun storage-info (&optional svc)
	; (storage-info [svc]) -> :nil | emap
	;what the store is: :id :member :members :block_size :copies :objects
	;:blocks :free
	(storage-info-read (storage-ask (ifn svc (storage-service)) +storage_type_info)))

(defun storage-stat (name &optional svc)
	; (storage-stat path [svc]) -> :nil | entry
	;what a path is now, :nil if there is no object of that name
	(and (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_stat name))
		(nempty? answer) (store-entry-read answer)))

(defun storage-list (&optional prefix svc)
	; (storage-list [prefix svc]) -> :nil | (entry ...)
	;the paths that begin so, in order
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_list (ifn prefix "")))
		(map (const store-entry-read) (filter (const nempty?) (split answer +storage_lf)))))

(defun storage-block-at (hash svc)
	; (storage-block-at name svc) -> :nil | data
	;a block by its name, if that service has it. It is checked against
	;its name here too, what came may not be what was sent
	(and (defq data (storage-ask svc +storage_type_block_get hash))
		(nempty? data) (eql (sha256 data) hash) data))

(defun storage-where (hash &optional svc)
	; (storage-where name [svc]) -> (netid ...)
	;the other members a block of that name may be had from, those that
	;are to keep it first. The service says who, it does not ask them
	(map (const hex-decode) (filter (const nempty?)
		(split (ifn (storage-ask (ifn svc (storage-service)) +storage_type_where hash) "") +storage_lf))))

(defun storage-block (hash &optional svc)
	; (storage-block name [svc]) -> :nil | data
	;a block by its name, from the service if it has it, and if not from
	;another member that has. The asker goes to the other member, a
	;service never asks another and waits: two that did, each of the
	;other, would wait for ever
	(setd svc (storage-service))
	(or (storage-block-at hash svc)
		(some (# (storage-block-at hash %0)) (storage-where hash svc))))

(defun storage-put-stream (name stream &optional from svc)
	; (storage-put-stream path stream [from svc]) -> (:ok entry) | (:stale entry) | (:full :nil) | (:lost :nil)
	;a path is set to an object of what is read from a stream, to its
	;end, provided it is now the object from: its name, "" for none, and
	;not given, not to say. :stale with what it is if it is not that. It
	;is read a few blocks at a time, so what is put can be of any length,
	;and only the blocks the service has not got are sent
	(setd svc (storage-service))
	(cond
		((not (defq info (storage-info svc))) (list :lost :nil))
		(:t (defq size (. info :find :block_size) names (list) bytes 0 full :nil more :t)
			(while (and more (not full))
				(defq blocks (list))
				(while (and (< (length blocks) 8) (setq more (read-blk stream size)))
					(push blocks more))
				(defq hashes (map (const sha256) blocks)
					has (if (empty? blocks) ""
						(ifn (storage-ask svc +storage_type_has (apply (const cat) (cat (list "") hashes))) "")))
				(each (lambda (block hash)
					(setq bytes (+ bytes (length block)))
					(push names hash)
					(unless (or full (eql (if (< (!) (length has)) (elem-get has (!))) "1"))
						(if (nql (storage-ask svc +storage_type_block_put block) hash) (setq full :t))))
					blocks hashes))
			(if full (list :full :nil)
				(storage-result (storage-ask svc +storage_type_commit
					(join (cat (list (storage-from from) (str bytes) name) (map (const hex-encode) names))
						+storage_lf)))))))

(defun storage-put (name data &optional from svc)
	; (storage-put path data [from svc]) -> (:ok entry) | (:stale entry) | (:full :nil) | (:lost :nil)
	;a path is set to an object of this data, as (storage-put-stream)
	(storage-put-stream name (string-stream data) from svc))

(defun storage-put-file (name file &optional from svc)
	; (storage-put-file path file [from svc]) -> (:ok entry) | (:stale entry) | (:full :nil) | (:lost :nil) | (:none :nil)
	;a path is set to an object of what is in a file of the host, of any
	;length. :none if there is no such file
	(if (defq stream (file-stream file))
		(storage-put-stream name stream from svc)
		(list :none :nil)))

(defun storage-manifest (name &optional svc)
	;the blocks of the object a path is, (bytes names), :nil if there is none
	(setd svc (storage-service))
	(and (defq entry (storage-stat name svc))
		(defq lines (filter (const nempty?) (split (ifn (storage-block (elem-get entry +store_entry_hash) svc) "") +storage_lf)))
		(> (length lines) 1)
		(list (str-as-num (second lines)) (map (const hex-decode) (slice lines 2 -1)))))

(defun storage-get (name &optional svc)
	; (storage-get path [svc]) -> :nil | data
	;all of an object. :nil if there is none, or a block of it can not be had
	(setd svc (storage-service))
	(when (defq object (storage-manifest name svc))
		(defq parts (list ""))
		(if (every (# (if (defq data (storage-block %0 svc)) (push parts data))) (second object))
			(apply (const cat) parts))))

(defun storage-read (name offset len &optional svc)
	; (storage-read path offset len [svc]) -> :nil | data
	;part of an object, only the blocks that part is in are asked for
	(setd svc (storage-service))
	(when (and (defq object (storage-manifest name svc)) (defq info (storage-info svc)))
		(defq size (. info :find :block_size) end (min (first object) (+ offset len))
			offset (max 0 (min offset end)) parts (list "") lost :nil at (/ offset size))
		(while (and (not lost) (< (* at size) end))
			(if (defq data (storage-block (elem-get (second object) at) svc))
				(push parts (slice data (max 0 (- offset (* at size))) (min (length data) (- end (* at size)))))
				(setq lost :t))
			(++ at))
		(unless lost (apply (const cat) parts))))

;a stream of an object. What it keeps as it is read: the service, the
;names of the blocks, how long a block is, how long the object is, the
;block that is next, how much of that one is not wanted, the blocks asked
;for and not yet read, how many are asked for ahead, and which of the
;blocks the service has
(enums +storage_stream 0
	(enum svc names block bytes at drop flight ahead has))

(defun storage-stream-ask (st at)
	; (storage-stream-ask st at) -> mbox
	;a block is asked for, and not waited for: of the service if it has
	;it, and if not of a member that keeps it, one after another of them
	;as the blocks go, so that what is ahead comes from several at once.
	;Where the answer will be
	(defq svc (elem-get st +storage_stream_svc) hash (elem-get (elem-get st +storage_stream_names) at)
		has (elem-get st +storage_stream_has))
	(unless (and (< at (length has)) (eql (elem-get has at) "1"))
		(if (nempty? (defq holders (storage-where hash svc)))
			(setq svc (elem-get holders (% at (length holders))))))
	(storage-send svc +storage_type_block_get hash))

(defun storage-stream-fill (st)
	; (storage-stream-fill st) -> :nil | data
	;the next block. The blocks after it are asked for as this one is
	;read, Chris: "We could implament read ahead at the point of
	;consumtion ?", so that they are here, or on their way, when wanted.
	;One that did not come, or is not what its name says, is had the slow
	;way, from any member that has it
	(bind '(svc names size bytes at drop flight ahead has) st)
	(defq next (+ at (length flight)))
	(while (and (< (length flight) ahead) (< next (length names)))
		(push flight (storage-stream-ask st next))
		(++ next))
	(when (nempty? flight)
		(defq hash (elem-get names at) data (mail-read-timeout (first flight) (task-timeout 2)))
		(elem-set st +storage_stream_flight (rest flight))
		(unless (and data (eql (sha256 data) hash))
			(setq data (storage-block hash svc)))
		(when data
			(elem-set st +storage_stream_at (inc at))
			(elem-set st +storage_stream_drop 0)
			(if (> drop 0) (slice data (min drop (length data)) -1) data))))

(defun storage-stream-seek (st offset whence)
	; (storage-stream-seek st offset whence) -> pos
	;it is read from there from now: from the start, from where it is, or
	;from the end. What was asked for ahead is not waited for, its
	;answers are to mailboxes nobody reads, and what is ahead of the new
	;place is asked for at the next read. That read waits for a block,
	;Chris: "a seek, pays the cost, but it allways does in any streaming
	;object"
	(bind '(svc names size bytes at drop flight ahead has) st)
	(defq pos (max 0 (min bytes (cond
		((= whence 0) offset)
		((= whence 1) (+ offset (min bytes (+ (* at size) drop))))
		((+ bytes offset))))))
	(elem-set st +storage_stream_at (/ pos size))
	(elem-set st +storage_stream_drop (% pos size))
	(elem-set st +storage_stream_flight (list))
	pos)

(defun storage-stream (name &optional svc ahead)
	; (storage-stream path [svc ahead]) -> :nil | stream
	;an object as a stream to read, as a file is, for what knows nothing
	;of a store: (read-line) (read-blk) (read-char) (stream-seek) and the
	;rest. It has a block of the object at a time, and the next few on
	;their way, ahead, 4 if not said: so the object can be of any size.
	;:nil if there is no object of that path
	(setd svc (storage-service) ahead 4)
	(when (and svc (defq object (storage-manifest name svc)) (defq info (storage-info svc)))
		(defq names (second object)
			st (list svc names (. info :find :block_size) (first object) 0 0 (list) (max 1 ahead)
				(ifn (storage-ask svc +storage_type_has (apply (const cat) (cat (list "") names))) "")))
		;the state is in the functions, a function has only what it is given
		(lisp-stream `(lambda () (,storage-stream-fill ',st))
			`(lambda (offset whence) (,storage-stream-seek ',st offset whence)))))

(defun storage-same-file? (name file &optional svc)
	; (storage-same-file? path file [svc]) -> :nil | bytes
	;an object, read as a stream, is what is in a file of the host, byte
	;for byte and no longer. How long that is
	(when (and (defq object (storage-stream name svc)) (defq host (file-stream file)))
		(defq bytes 0 same :t)
		(while (and same (defq part (read-blk object 65536)))
			(setq same (eql part (read-blk host (length part))) bytes (+ bytes (length part))))
		(and same (not (read-char host)) bytes)))

(defun storage-delete (name &optional from svc)
	; (storage-delete path [from svc]) -> (:ok entry) | (:stale entry) | (:none :nil) | (:lost :nil)
	;there is no object of that name from now, provided it is from
	(storage-result (storage-ask (ifn svc (storage-service)) +storage_type_delete
		(cat (storage-from from) +storage_lf name))))

(defun storage-copies (name &optional copies svc)
	; (storage-copies path [copies svc]) -> :nil | num | (:ok entry) | (:none :nil) | (:lost :nil)
	;how many members are to keep the blocks of the object a path is, the
	;dial: more for what must not be lost, or is read a great deal, fewer
	;for what can be had again. 0 is as many as the store says. It is a
	;write of the path, and the members come to it as they hear. Without
	;copies, how many it is now, :nil if there is no such object
	(setd svc (storage-service))
	(cond
		(copies (storage-result (storage-ask svc +storage_type_copies (cat (str copies) +storage_lf name))))
		((defq entry (storage-stat name svc))
			(if (> (defq said (elem-get entry +store_entry_copies)) 0) said
				(and (defq info (storage-info svc)) (. info :find :copies))))))

(defun storage-walk (name state visit &optional at wait svc)
	; (storage-walk path state visit [at wait svc]) -> :nil | (state trail)
	; visit is '(lambda (state data at count) ... (list state next))
	;a gopher is sent to an object. It has what it was given, state, and
	;a function, visit, and goes to the
	;blocks of the object one at a time, the first, or at: the function is
	;given what it has, the block, which block it is and how many there
	;are, and gives what it has now and the block to go to next, :nil when
	;it is done. What it has then comes straight back here, with its
	;trail, ((member at) ...), the members it was at in the order it went
	;to them and the block it came to each for: where
	;a block it wants is not kept by the member it is at, it is sent on,
	;as it is, to one that keeps it, and carries on there.
	;:nil if there is no such object, a block it went to could not be had,
	;the function threw, or nothing came back in the time, seconds.
	;
	;visit is the function written out, quoted, not a function: it is
	;sent as text, with what it has, and that is all there is of the
	;gopher, so it can be sent on to wherever the next block is. What it
	;has must print as it reads, numbers, strs, lists of them.
	;
	;Each member has a task that runs the gophers that are at it, a block
	;of each in turn, ./gopher.lisp: a function that does not come back
	;holds up the others that are there
	(defq mbox (mail-mbox) svc (ifn svc (storage-service)))
	(when svc
		;who it is from, - for not another member, and all there is of it
		(mail-send svc (cat (setf-> (str-alloc +storage_rpc_size)
				(+storage_rpc_reply_id mbox) (+storage_rpc_type +storage_type_walk))
			"-" +storage_lf (str (list name (ifn at 0) state visit (list) (list)))))
		(and (defq answer (mail-read-timeout mbox (task-timeout (ifn wait +storage_wait))))
			(starts-with "(" answer)
			(first (read (string-stream answer))))))

(defun storage-pin (prefix &optional off svc)
	; (storage-pin prefix [off svc]) -> :nil | (prefix ...)
	;this system holds all of every object whose path begins so, or with
	;off no longer does. What is pinned now
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_pin (cat (if off "-" "+") prefix)))
		(filter (const nempty?) (split answer +storage_lf))))

(defun storage-pinned-by (name &optional svc)
	; (storage-pinned-by path [svc]) -> :nil | (member ...)
	;the members that have that path pinned, that hold all of its object,
	;as far as this system knows. Every member is told what each has pinned
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_pinned name))
		(filter (const nempty?) (split answer " "))))

(defun storage-pins-missing (&optional svc)
	; (storage-pins-missing [svc]) -> :nil | (name ...)
	;the blocks of what is pinned that this system has not got yet, in
	;hex. None, and it is all here
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_pins_missing))
		(filter (const nempty?) (split answer +storage_lf))))

(defun storage-sweep (&optional svc)
	; (storage-sweep [svc]) -> :nil | num
	;the blocks no object is made of are let go of, how many
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_sweep "" 60)) (str-as-num answer)))

(defun storage-check (&optional mend svc)
	; (storage-check [mend svc]) -> :nil | (name ...)
	;the blocks that are not what their names say, in hex. With mend they
	;are let go of
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_check (if mend "mend" "") 120))
		(filter (const nempty?) (split answer +storage_lf))))

(defun storage-start (image &rest opts)
	; (storage-start image [:size bytes :id str :name str :block bytes :force :t
	;	:group str :member str :tick us :grace us :copies num :node node]) -> :nil | netid
	;the service is started on this node, or that one, to keep the store that is in
	;that file of the host. If there is none there and a size is given,
	;one is made of that many bytes, the store id, and this system its
	;member. :nil if there is no store and none could be made. What is
	;there and is not a store is left as it is, unless force: then it is
	;made one, and what was there is gone, a disk that is to be a store.
	;If the service of that name is there already, it is that one.
	;
	;name is what it is known by on this system, @Storage. group is what
	;the members of a store find each other by, over every machine,
	;*Storage. member is who it is, this system: another only for a test
	;of several members on the one machine. tick is how often its tender
	;looks, 0 for only when it is told to, and grace how long a member is
	;not heard from before what it kept is kept by others
	(defq opts (apply (const scatter) (cat (list (Emap)) opts)) name (ifn (. opts :find :name) +storage_name))
	(cond
		((storage-service name))
		(:t (defq said (mail-mbox) node (. opts :find :node)
				task (if node (open-remote "service/storage/app.lisp" node +kn_call_pin)
					(open-child "service/storage/app.lisp" +kn_call_pin)))
			(when (/= 0 (get-long task 0))
				(defq lines (list (cat "ready " (hex-encode said)) (cat "image " image) (cat "name " name)))
				(each (lambda (key)
					(if (defq val (. opts :find key)) (push lines (cat (rest key) " " (if (eql val :t) "1" (str val))))))
					'(:size :id :block :force :group :member :tick :grace :copies))
				(mail-send task (join lines +storage_lf))
				(when (eql (mail-read-timeout said (task-timeout 30)) "ok")
					;on another node, its name is known here a moment after
					(defq t0 (pii-time))
					(until (or (defq svc (storage-service name)) (> (- (pii-time) t0) (task-timeout 5)))
						(task-sleep 10000))
					svc)))))

(defun storage-stop (&optional svc)
	; (storage-stop [svc]) -> :nil | :t
	;the service puts what it has on the volume and goes
	(if (storage-ask (ifn svc (storage-service)) +storage_type_stop) :t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a store of more than one member
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun storage-state-read (answer)
	; (storage-state-read answer) -> :nil | emap
	;what a service said of itself, as (storage-state) gives it
	(when answer
		(defq state (Emap))
		(each (lambda (line)
			(defq at (ifn (find " " line) (length line)) key (sym (cat ":" (slice line 0 at)))
				val (if (< at (length line)) (slice line (inc at) -1) ""))
			(. state :insert key (if (find key '(:members :retired :cut)) (split val " ") val)))
			(split answer +storage_lf))
		state))

(defun storage-state (&optional svc)
	; (storage-state [svc]) -> :nil | emap
	;what a member is, as its tender and other members ask it: :member
	;:store :digest :members :retired :cut :tender
	(storage-state-read (storage-ask (ifn svc (storage-service)) +storage_type_state)))

(defun storage-members (&optional change svc)
	; (storage-members [change svc]) -> :nil | (member ...)
	;who the members of the store are meant to be. With a change, "+id"
	;a machine is joined to it, "-id" one is retired: it is told to this
	;member, and the others hear of it as they meet
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_members (ifn change "")))
		(split answer " ")))

(defun storage-seen (name &optional svc)
	; (storage-seen path [svc]) -> :nil | (seen members)
	;how many of the members are known to have what a path is now, and
	;how many members there are: seen by 4 of 5
	(setd svc (storage-service))
	(and (defq answer (storage-ask svc +storage_type_stat (cat "?" name))) (nempty? answer)
		(defq entry (store-entry-read answer)) (defq members (storage-members :nil svc))
		(list (length (filter (# (find %0 members)) (elem-get entry +store_entry_seen))) (length members))))

(defun storage-wait (name &optional wait want svc)
	; (storage-wait path [wait want svc]) -> :nil | (seen members)
	;wait till most of the members have what a path is now, or as many as
	;want, and not longer than wait, seconds, 10. :nil if they have not by
	;then: a member that is cut off from most of the others has that
	;every time, and that is how a writer knows.
	;
	;A write is taken by the one member whoever can be reached. One that
	;most members have keeps its path when members that were apart meet,
	;(store-keeps), so a writer that must not have its write set aside
	;waits for this, and takes :nil as not written
	(setd svc (storage-service) wait 10)
	(defq t0 (pii-time) seen (storage-seen name svc))
	(while (and seen (not (>= (first seen) (ifn want (inc (/ (second seen) 2)))))
			(< (- (pii-time) t0) (task-timeout wait)))
		(task-sleep 100000)
		(setq seen (storage-seen name svc)))
	(and seen (>= (first seen) (ifn want (inc (/ (second seen) 2)))) seen))

(defun storage-settle (&optional svc wait)
	; (storage-settle [svc wait]) -> :nil | (met fetched dropped)
	;the member's tender is told to look now, and what it did is waited
	;for: how many members it compared notes with and found something
	;new, how many blocks it fetched, and how many it let go of
	(when (and (defq state (storage-state svc)) (nempty? (defq tender (. state :find :tender))))
		(defq mbox (mail-mbox))
		(mail-send (hex-decode tender) (cat "pass " (hex-encode mbox)))
		(if (defq answer (mail-read-timeout mbox (task-timeout (ifn wait 60))))
			(map (const str-as-num) (split answer " ")))))

(defun storage-pace (tick grace &optional svc)
	; (storage-pace tick grace [svc]) -> :nil | :t
	;how often the member's tender looks, 0 for only when told, and how
	;long a member is not heard from before what it kept is kept by
	;others, from now on
	(when (and (defq state (storage-state svc)) (nempty? (defq tender (. state :find :tender))))
		(mail-send (hex-decode tender) (cat "pace " (str tick) " " (str grace)))
		:t))

(defun storage-cut (members &optional svc)
	; (storage-cut members [svc]) -> :nil | :t
	;for a test: the member is as if it could not reach those members,
	;its tender does not look at them. None, and it can reach all again
	(if (storage-ask (ifn svc (storage-service)) +storage_type_cut (join members " ")) :t))
