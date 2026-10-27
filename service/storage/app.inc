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
		copies pinned
		;a recorder that is to be told what is done, see below
		record
		;snapshots, lib/storage/store.inc
		snapshot restore snapshot_drop
		;a store moved to a volume of another size
		resize
		;a path given the object of another, which is then no more
		rename))

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
	(mail-first (ifn name +storage_name)))

(defun storage-service-on (system)
	; (storage-service-on system) -> :nil | netid
	;the Storage service of another machine, by the start of its system id
	;in hex, as mesh lists it: what is in its store looked at, put or got,
	;as a client, this machine no member of it
	(defq system (to-upper system))
	(some (lambda (entry)
			(defq svc (mail-entry-id entry)
				node (slice svc +long_size (+ +long_size +node_id_size)))
			(if (some (# (and (starts-with system (to-upper (hex-encode %0))) (find node (lisp-nodes %0)))) (lisp-systems))
				svc))
		(mail-enquire (cat +storage_group ","))))

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
		((eql what "missing") (list :missing (map! (const hex-decode) (list lines) 1 -1)))
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

(defun storage-has (svc names)
	; (storage-has svc names) -> str
	;which of these blocks the service has, "1" or "0" for each, asked a
	;manifest block's worth at a time, so no one ask is big. "0" for those
	;it did not answer
	(defq out (list "") at 0)
	(while (< at (length names))
		(defq part (slice names at (min (length names) (+ at +store_manifest_names)))
			has (ifn (storage-ask svc +storage_type_has (apply (const cat) (cat (list "") part))) ""))
		(push out (if (= (length has) (length part)) has (pad "" (length part) "0")))
		(setq at (+ at +store_manifest_names)))
	(apply (const cat) out))

(defun storage-commit (svc name from bytes names)
	; (storage-commit svc path from bytes names) -> (:ok entry) | (:stale entry) | (:missing names) | (:full :nil) | (:lost :nil)
	;a path set to the object of these blocks, all put already. The blocks
	;of its manifest are put as blocks, (store-manifest-make), and only the
	;name of its top is sent: no message is the whole of a big object. A
	;block that was not answered is :lost, one there was no room for :full
	(defq full :nil
		top (store-manifest-make bytes names (lambda (data)
			(defq hash (sha256 data) said (storage-ask svc +storage_type_block_put data))
			(cond ((eql said hash) hash) (said (setq full :t) :nil)))))
	(cond
		(full (list :full :nil))
		((not top) (list :lost :nil))
		((storage-result (storage-ask svc +storage_type_commit
			(join (list (storage-from from) (str bytes) name (hex-encode top)) +storage_lf))))))

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
		(:t (defq size (. info :find :block_size) names (list) bytes 0 full :nil lost :nil more :t)
			(while (and more (not full) (not lost))
				(defq blocks (list))
				(while (and (< (length blocks) 8) (setq more (read-blk stream size)))
					(push blocks more))
				(defq hashes (map (const sha256) blocks)
					has (if (empty? blocks) "" (storage-has svc hashes)))
				(each (lambda (block hash)
					(setq bytes (+ bytes (length block)))
					(push names hash)
					(unless (or full lost (eql (if (< (!) (length has)) (elem-get has (!))) "1"))
						;no room is the service saying so, no answer is not
						(defq said (storage-ask svc +storage_type_block_put block))
						(cond ((eql said hash)) ((not said) (setq lost :t)) ((setq full :t)))))
					blocks hashes))
			(cond
				(full (list :full :nil))
				(lost (list :lost :nil))
				((storage-commit svc name from bytes names))))))

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
	;the blocks of the object a path is, (bytes names tree), :nil if there
	;is none. Its manifest is had a block at a time, a big one's is a tree
	;of them, (store-manifest-walk)
	(setd svc (storage-service))
	(and (defq entry (storage-stat name svc))
		(store-manifest-walk (elem-get entry +store_entry_hash) (# (storage-block %0 svc)))))

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
			offset (clamp offset 0 end) parts (list "") lost :nil at (/ offset size))
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
;blocks the service has, and for how many seconds one that can not be
;had is asked for again; the last block read, (index data), and where in
;it the next read is from when a seek went back into it
(enums +storage_stream 0
	(enum svc names block bytes at drop flight ahead has retry kept again))

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

;how long a stream that is read tries for a block it can not get
(defq +storage_retry 30)

(defun storage-block-retry (hash svc secs)
	; (storage-block-retry name svc secs) -> :nil | data
	;a block by its name, of any member that has it, asked again each
	;second till there is one, or for secs seconds
	(defq t0 (pii-time) data :nil)
	(until (or (setq data (storage-block hash svc)) (> (- (pii-time) t0) (task-timeout secs)))
		(task-sleep 1000000))
	data)

(defun storage-stream-next (st)
	; (storage-stream-next st) -> :nil | data | (:lost hex)
	;the next block, had of the service, (storage-stream-fill)
	(bind '(svc names size bytes at drop flight ahead has retry &ignore) st)
	(defq next (+ at (length flight)))
	(while (and (< (length flight) ahead) (< next (length names)))
		(push flight (storage-stream-ask st next))
		(++ next))
	(when (nempty? flight)
		(defq hash (elem-get names at) data (mail-read-timeout (first flight) (task-timeout 2)))
		(elem-set st +storage_stream_flight (rest flight))
		(unless (and data (eql (sha256 data) hash))
			(setq data (storage-block-retry hash svc retry)))
		(cond
			((not data) (list :lost (hex-encode hash)))
			(:t (elem-set st +storage_stream_at (inc at))
				(elem-set st +storage_stream_drop 0)
				(elem-set st +storage_stream_kept (list at data))
				(if (> drop 0) (slice data (min drop (length data)) -1) data)))))

(defun storage-stream-fill (st)
	; (storage-stream-fill st) -> :nil | data | (:lost hex)
	;the next block. The blocks after it are asked for as this one is
	;read, Chris: "We could implament read ahead at the point of
	;consumtion ?", so that they are here, or on their way, when wanted.
	;One that did not come, or is not what its name says, is had the slow
	;way, from any member that has it, again and again for a while, a link
	;that dropped is made again in a few seconds. One that can not be had
	;is the stream's fault, (:lost hex) with its name, so that a reader can
	;know it is not the end of the object, (stream-fault)
	(bind '(svc names size bytes at drop flight ahead has retry kept again) st)
	(cond
		;a seek back into the block last read: it is read again from there,
		;nothing asked for
		(again (elem-set st +storage_stream_again :nil)
			(slice (second kept) (min again (length (second kept))) -1))
		((storage-stream-next st))))

(defun storage-stream-seek (st offset whence)
	; (storage-stream-seek st offset whence) -> pos
	;it is read from there from now: from the start, from where it is, or
	;from the end. What was asked for ahead is not waited for, its
	;answers are to mailboxes nobody reads, and what is ahead of the new
	;place is asked for at the next read. That read waits for a block,
	;Chris: "a seek, pays the cost, but it allways does in any streaming
	;object"
	(bind '(svc names size bytes at drop flight ahead has retry kept &ignore) st)
	(defq pos (max 0 (min bytes (cond
		((= whence 0) offset)
		((= whence 1) (+ offset (min bytes (+ (* at size) drop))))
		((+ bytes offset)))))
		block (/ pos size))
	(elem-set st +storage_stream_again :nil)
	(cond
		;into the block last read: had again from it, and what is on its way
		;is still what comes after. An MP3 seeks twice a frame of a few
		;hundred bytes
		((and (= block (first kept)) (< pos bytes))
			(elem-set st +storage_stream_again (% pos size)))
		;to the next block: what is on its way is still wanted
		((= block at)
			(elem-set st +storage_stream_drop (% pos size)))
		;anywhere else, the cost of a seek: what is kept is not the block
		;before the next one now
		(:t (elem-set st +storage_stream_at block)
			(elem-set st +storage_stream_drop (% pos size))
			(elem-set st +storage_stream_flight (list))
			(elem-set st +storage_stream_kept (list -1 ""))))
	pos)

;a stream of an object that is written. What it keeps: the service, the
;path, how long a block is, the names of the whole blocks put so far,
;what is written and not yet a whole block, and if there is anything the
;path has not been set to
;and the blocks on their way, a few at once, (mbox hash), the oldest first
(enums +storage_out 0
	(enum svc name block names part dirty flight))

;how many blocks a stream that is written has on their way at once
(defq +storage_put_ahead 4)

(defun storage-stream-land (st most)
	; (storage-stream-land st most) -> st
	;the answers to the blocks on their way, the oldest first, till no more
	;than most are. One not put as it should be, and the path is never set
	(defq flight (elem-get st +storage_out_flight))
	(while (> (length flight) most)
		(bind '(mbox hash) (first flight))
		(unless (eql (mail-read-timeout mbox (task-timeout +storage_wait)) hash)
			(elem-set st +storage_out_dirty :lost))
		(setq flight (rest flight)))
	(elem-set st +storage_out_flight flight)
	st)

(defun storage-stream-put (st data flush)
	; (storage-stream-put st data flush) -> :nil | (:ok entry) | (:stale entry) | (:full :nil) | (:lost :nil)
	;more of an object that is written, and if it is a flush. Each whole
	;block is put as there is one, so what is written can be of any
	;length. At a flush the path is set to the object of the blocks so
	;far and what is left over, a last short block: till then it is what
	;it was, and one who reads it sees the old or the new and never a
	;part. What is left over is kept, a stream can be written on after
	;a flush, and the next sets the path again
	(bind '(svc name size names part dirty &ignore) st)
	(when (nempty? data)
		(setq part (cat part data))
		(if (nql dirty :lost) (elem-set st +storage_out_dirty :t))
		(while (>= (length part) size)
			;sent, and hashed while the service has it: a few on their way
			;at once, and what each is said to be is looked at in turn. No
			;room for one, or nothing said: the path is not set, ever
			(defq block (slice part 0 size) mbox (storage-send svc +storage_type_block_put block)
				hash (sha256 block))
			(push (elem-get st +storage_out_flight) (list mbox hash))
			(push names hash)
			(storage-stream-land st +storage_put_ahead)
			(setq part (slice part size -1)))
		(elem-set st +storage_out_part part))
	;all of them answered before a flush sets the path
	(when flush
		(storage-stream-land st 0)
		(setq dirty (elem-get st +storage_out_dirty)))
	(when (and flush (eql dirty :t))
		(defq all (cat names) bytes (+ (* (length names) size) (length part)))
		(when (or (empty? part) (and (eql (storage-ask svc +storage_type_block_put part) (defq hash (sha256 part)))
				(push all hash)))
			(elem-set st +storage_out_dirty :nil)
			(storage-commit svc name :nil bytes all))))

(defun storage-stream (name &optional mode svc ahead)
	; (storage-stream path [mode svc ahead]) -> :nil | stream
	;an object as a stream, as a file is, for what knows nothing of a
	;store, mode as (file-stream) has it.
	;
	;+file_open_read, or no mode: to read, (read-line) (read-blk)
	;(read-char) (stream-seek) and the rest. It has a block of the object
	;at a time, and the next few on their way, ahead, 4 if not said: so
	;the object can be of any size. :nil if there is no object of that
	;path.
	;
	;+file_open_write: to write a new object, of nothing to begin with.
	;+file_open_append: to write on the end of the object that is there,
	;or a new one if there is none. The path is set to what was written
	;when the stream is flushed, (stream-flush), and when it is let go
	;of, as a file is: not before, and whoever sets it last has it. One
	;opened to write and let go of with nothing written is an object of
	;nothing, as a file would be. :nil if there is no service
	(setd svc (storage-service) ahead 4 mode +file_open_read)
	(cond
		((not (and svc (defq info (storage-info svc)))) :nil)
		((= mode +file_open_read)
			(when (defq object (storage-manifest name svc))
				(defq names (second object)
					st (list svc names (. info :find :block_size) (first object) 0 0 (list) (max 1 ahead)
						(storage-has svc names) +storage_retry
						(list -1 "") :nil))
				;the state is in the functions, a function has only what it is given
				(lisp-stream `(lambda () (,storage-stream-fill ',st))
					`(lambda (offset whence) (,storage-stream-seek ',st offset whence)))))
		(:t (defq size (. info :find :block_size) names (list) part "" dirty (= mode +file_open_write))
			(when (and (= mode +file_open_append) (defq object (storage-manifest name svc)))
				;the blocks that are whole are had by their names, they are
				;not read. A last short one is read, to be written on
				(setq names (cat (second object)))
				(and (nempty? names) (/= 0 (% (first object) size))
					(setq part (ifn (storage-block (pop names) svc) "") dirty (if (empty? part) :lost dirty))))
			(defq st (list svc name size names part dirty (list)))
			(lisp-stream '(lambda () :nil) :nil
				`(lambda (data flush) (,storage-stream-put ',st data flush))))))

(defun storage-same-file? (name file &optional svc)
	; (storage-same-file? path file [svc]) -> :nil | bytes
	;an object, read as a stream, is what is in a file of the host, byte
	;for byte and no longer. How long that is
	(when (and (defq object (storage-stream name +file_open_read svc)) (defq host (file-stream file)))
		(defq bytes 0 same :t)
		(while (and same (defq part (read-blk object 65536)))
			(setq same (eql part (read-blk host (length part))) bytes (+ bytes (length part))))
		(and same (not (read-char host)) bytes)))

(defun storage-delete (name &optional from svc)
	; (storage-delete path [from svc]) -> (:ok entry) | (:stale entry) | (:none :nil) | (:lost :nil)
	;there is no object of that name from now, provided it is from
	(storage-result (storage-ask (ifn svc (storage-service)) +storage_type_delete
		(cat (storage-from from) +storage_lf name))))

(defun storage-rename (from to &optional was svc)
	; (storage-rename from to [was svc]) -> (:ok entry) | (:stale entry) | (:none :nil) | (:lost :nil)
	;the path to is given the object the path from is, and from is then no
	;more, in one step: nothing is copied, a path is only the name of the
	;object it is. Provided to is now the object was, "" for none, :nil not
	;to say, :stale with what it is if it is not. :none if from is not.
	;What a program does that is to come through anything: write a path of
	;its own, see that all went as it should, and rename it to where it is
	;to be, and who reads that has the old object or the new, whole
	(storage-result (storage-ask (ifn svc (storage-service)) +storage_type_rename
		(cat (storage-from was) +storage_lf from +storage_lf to))))

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

(defun storage-walk-send (name state visit &optional at svc)
	; (storage-walk-send path state visit [at svc]) -> :nil | mbox
	;a gopher sent, as (storage-walk), and not waited for: where what it
	;comes back with will be, for (storage-walk-answer). So many can be
	;out at once
	(defq mbox (mail-mbox) svc (ifn svc (storage-service)))
	(when svc
		;who it is from, - for not another member, and all there is of it
		(mail-send svc (cat (setf-> (str-alloc +storage_rpc_size)
				(+storage_rpc_reply_id mbox) (+storage_rpc_type +storage_type_walk))
			"-" +storage_lf (str (list name (ifn at 0) state visit (list) (list)))))
		mbox))

(defun storage-walk-answer (mbox &optional wait)
	; (storage-walk-answer mbox [wait]) -> :nil | (state trail)
	;what a gopher sent with (storage-walk-send) came back with, as
	;(storage-walk), waited for so many seconds at most, or with :t till
	;it comes
	(and mbox (defq answer (if (eql wait :t) (mail-read mbox) (mail-read-timeout mbox (task-timeout (ifn wait +storage_wait)))))
		(starts-with "(" answer)
		(first (read (string-stream answer)))))

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
	;of each in turn, ./gopher.lisp. A function has a second for a block:
	;it is made to give way and to look at the clock each time it goes
	;round, and one that has had its second is done with, :nil comes back.
	;
	;The rules of ChrysaLisp are the rules of a visit: it does not call
	;itself. It goes round with a loop, or by asking for the block again
	(storage-walk-answer (storage-walk-send name state visit at svc) wait))

(defun storage-pin (prefix &optional off svc)
	; (storage-pin prefix [off svc]) -> :nil | (prefix ...)
	;this system holds all of every object whose path begins so, or with
	;off no longer does. What is pinned now
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_pin (cat (if off "-" "+") prefix)))
		(filter (const nempty?) (split answer +storage_lf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this machine as a member, by itself
;
; A machine is made a member the once, by somebody: (storage-join), the
; storage command's -j. What it was told is kept in a file at the root of
; the tree, as the key of the mesh is, and is of the machine and not of
; a user. From then on the machine's store is started whenever the
; session that is the machine's member of the mesh comes up,
; ./auto.lisp, which the Net service starts when it has the port: only
; one process can listen on a port, so only one session serves the
; store, and that is settled by the host and can not be raced.
;
; Asked for by hand in another session, what holds two off the one image
; is a file beside it with the id of the process that has it. One that
; is of a process that has gone is nobody's.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +storage_member_file "store_member" +storage_image "storage/store.img"
	;how big a store is made if it is not said, MB. It is the most it may
	;ever hold: the file takes only what is in it, on a file system that
	;lets a file have holes
	+storage_size 4096)

(defun storage-key-id (&optional file)
	; (storage-key-id [file]) -> :nil | id
	;the id of the store of the mesh this machine is of: made from its
	;key, so machines given the same key are of the same store with
	;nothing said, and not the key, nor anything the key can be had from.
	;:nil if the machine has no key
	(and (defq text (load (ifn file "mesh_key")))
		(nempty? (setq text (trim text (const (char-class " \t\r\n")))))
		(cat "mesh-" (slice (hex-encode (sha256 (cat "ChrysaLisp store of " text))) 0 16))))

(defun storage-member (&optional file)
	; (storage-member [file]) -> :nil | emap
	;what this machine was told when it was made a member: :image, :size
	;in MB, :id and :copies if they were said, and :travels, :t if it is
	;a machine that is taken away from the others, a laptop. :nil if it
	;never was a member
	(when (defq text (load (ifn file +storage_member_file)))
		(defq said (Emap))
		(each (lambda (line)
			(when (defq at (find " " line))
				(defq key (slice line 0 at) val (slice line (inc at) -1))
				(cond
					((eql key "image") (. said :insert :image val))
					((eql key "size") (. said :insert :size (str-as-num val)))
					((eql key "id") (. said :insert :id val))
					((eql key "copies") (. said :insert :copies (str-as-num val)))
					((eql key "travels") (. said :insert :travels (eql val "yes"))))))
			(split text +storage_lf))
		(if (. said :find :image) said)))

(defun storage-member-save (image size &optional id copies file travels)
	; (storage-member-save image size [id copies file travels])
	;this machine is a member, of the store in that file, from now on
	(save (join (cat (list (cat "image " image) (cat "size " (str size)))
			(if id (list (cat "id " id)) (list))
			(if (and copies (> copies 0)) (list (cat "copies " (str copies))) (list))
			(if travels (list "travels yes") (list))
			(list "")) +storage_lf)
		(ifn file +storage_member_file)))

(defun storage-member-travels (travels &optional file)
	; (storage-member-travels travels [file]) -> :nil | :t
	;this machine is one that is taken away from the others, or is not
	;now. :nil if it is no member
	(when (defq said (storage-member file))
		(storage-member-save (. said :find :image) (. said :find :size) (. said :find :id)
			(. said :find :copies) file travels)
		:t))

(defun storage-user-pin (&optional svc file)
	; (storage-user-pin [svc file]) -> :nil | prefix
	;on a machine that travels, the area of the user of this session,
	;usr/<user>/, is pinned here, so that when it is taken away it has all
	;of it. The user is the session's own, run.sh -u name, or the one the
	;machine has, usr/current. :nil if it does not travel, or there is no
	;user, or no service
	(and (defq said (storage-member file)) (. said :find :travels)
		(nempty? (defq user (trim (ifn (get '*env_node_user*) (ifn (load "usr/current") "")) (const (char-class " \t\r\n")))))
		(storage-pin (defq prefix (cat "usr/" user "/")) :nil (ifn svc (storage-service)))
		prefix))

(defun storage-member-forget (&optional file)
	; (storage-member-forget [file])
	;this machine is not started as a member by itself any more. Its
	;store is left as it is
	(if (/= 0 (age (ifn file +storage_member_file))) (pii-remove (ifn file +storage_member_file))))

(defun storage-claim (image)
	; (storage-claim image) -> :nil | :t
	;is the store in that file this process's to serve. It is if no
	;process that is still there has said it is its own. It says so, in
	;a file beside the image, waits a moment, and looks again: of two
	;that say so at once the last to write is what both read, so one of
	;them goes on and the other does not
	(defq note (cat image ".member") me (str (pii-pid))
		held (if (defq text (load note)) (trim text (const (char-class " \t\r\n"))) ""))
	(cond
		((and (nempty? held) (nql held me) (pii-alive (str-as-num held))) :nil)
		(:t (save me note)
			(task-sleep 50000)
			(eql (if (defq text (load note)) (trim text (const (char-class " \t\r\n"))) "") me))))

(defun storage-unclaim (image)
	; (storage-unclaim image)
	;the store in that file is nobody's to serve, if it was this process's
	(defq note (cat image ".member"))
	(and (defq text (load note)) (eql (trim text (const (char-class " \t\r\n"))) (str (pii-pid)))
		(pii-remove note)))

(defun storage-join (&optional image size id copies force wait stop)
	; (storage-join [image size id copies force wait stop]) -> :nil | netid
	;the store of this machine is started, in this session: the one in
	;that file, storage/store.img if it is not said, made if there is
	;none, of so many MB. Its id is what is said, or the mesh's own,
	;(storage-key-id), or store. :nil if another process has it, or it
	;could not be started, and then it is not claimed. wait and stop are
	;as (storage-start) has them
	(setd image +storage_image size +storage_size)
	(and (storage-claim image)
		;it is defined after this, so by its name as this is called, not as it is read
		(or (apply storage-start (cat (list image :size (* size 1048576) :id (ifn id (ifn (storage-key-id) "store")))
				(if (and copies (> copies 0)) (list :copies copies) (list))
				(if force (list :force :t) (list))
				(if wait (list :wait wait) (list))
				(if stop (list :stop stop) (list))))
			(progn (storage-unclaim image) :nil))))

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
	;	:group str :member str :tick us :grace us :copies num :node node :wait s
	;	:stop mbox]) -> :nil | netid
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
	;not heard from before what it kept is kept by others.
	;
	;wait is how long it is given to say it is up, 30 s, or :t, as long
	;as it takes while its task is there, or its node, on another: a big
	;store is minutes opening, and one given up on comes up all the same.
	;Mail to stop, a mailbox of the caller, while it waits, and the service
	;is told to give up its open: the caller is to go, and the store is to
	;be another's. That mail is left there for the caller to read
	;
	;node is the node it is started on, this one if not said. Another
	;was tried, so that the hashing of a block and its writing were done
	;beside what the asker does: each block's crossing of the link cost
	;more than the second core gave, a copy into a store fell 61 to 50
	;MB/s, a read from it 116 to 60
	(defq opts (apply (const scatter) (cat (list (Emap)) opts)) name (ifn (. opts :find :name) +storage_name))
	(cond
		((storage-service name))
		(:t (defq said (mail-mbox) here (task-nodeid) node (. opts :find :node)
				task (if node (open-remote "service/storage/app.lisp" node +kn_call_pin)
					(open-child "service/storage/app.lisp" +kn_call_pin)))
			(when (/= 0 (get-long task 0))
				(defq lines (list (cat "ready " (hex-encode said)) (cat "image " image) (cat "name " name)))
				(each (lambda (key)
					(if (defq val (. opts :find key)) (push lines (cat (rest key) " " (if (eql val :t) "1" (str val))))))
					'(:size :id :block :force :group :member :tick :grace :copies))
				(mail-send task (join lines +storage_lf))
				(defq wait (ifn (. opts :find :wait) 30) answer :nil stop (. opts :find :stop))
				;a task of this node can be asked if it is there, one of
				;another only if its node is
				(until (or (progn
							(when (and stop (not (mail-poll (list said))) (mail-poll (list stop)))
								(mail-send task "stop")
								(setq stop :nil))
							(setq answer (mail-read-timeout said (task-timeout (if (eql wait :t) 1 wait)))))
					(nql wait :t)
					(if (eql (task-nodeid task) here) (not (mail-validate task))
						(not (find (task-nodeid task) (lisp-nodes))))))
				;the task's mailbox is the service's. Its name, from another
				;node, is known here a moment after, a second or more: it is
				;waited for, a start that asks again by the name finds this
				;one, but not for ever, the service is up all the same
				(when (eql answer "ok")
					(defq t0 (pii-time))
					(until (or (storage-service name) (> (- (pii-time) t0) (task-timeout 10)))
						(task-sleep 10000))
					task)))))

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

(defun storage-retire (start &optional svc)
	; (storage-retire start [svc]) -> (:ok member ...) | (:none) | (:many id ...) | :nil
	;the member whose id starts so retired, case aside, as (storage-service-on)
	;takes the start of a system id. Only if one member's id starts so:
	;:none if none does, and :many and theirs if more than one, none retired
	(setd svc (storage-service))
	(when (defq members (storage-members :nil svc))
		(defq start (to-upper start) hits (filter (# (starts-with start (to-upper %0))) members))
		(case (length hits)
			(0 (list :none))
			(1 (cat (list :ok) (ifn (storage-members (cat "-" (first hits)) svc) (list))))
			(:t (cat (list :many) hits)))))

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

;;;;;;;;;;;;;;
; the recorder
;;;;;;;;;;;;;;
;
; What a member does can be recorded. Its service, its tender and its
; gophers each say what they did, a line each, to a mailbox that has
; asked to be told, a recorder. A line is the time in microseconds, the
; member, what was done, and what it was done to, a path last, it may
; have spaces in it:
;
;	1791904249088123 m-0 set 1a2b3c4d 5120 tree/apps/demos/app.lisp
;
; A recorder asks each member to tell it, and is told for so long,
; +storage_record_lease, then no more unless it has asked again: one
; that has gone is not told for long. There can be several, an app and a
; terminal say, and each is told all of it. A task says what it did when
; it has done a thing asked of it, all of it in one mail to each. With no
; recorder nothing is said, and nothing is made to be said.

(defq +storage_record_lease 5000000)

(defun storage-record-new ()
	; (storage-record-new) -> rec
	;what a task of a member keeps of its recorders: each, (mbox until),
	;and the lines it has still to send
	(list (list) (list)))

(defun storage-record-to (rec text)
	; (storage-record-to rec text) -> rec
	;a recorder, the mailbox in hex, is told from now, or for longer. -
	;and a mailbox, that one is not, and - alone, none is
	(defq them (first rec))
	(cond
		((eql text "-") (setq them (list)))
		((starts-with "-" text)
			(defq mbox (hex-decode (rest text)))
			(setq them (filter (# (nql (first %0) mbox)) them)))
		(:t (defq mbox (hex-decode text))
			(setq them (cat (filter (# (nql (first %0) mbox)) them)
				(list (list mbox (+ (pii-time) +storage_record_lease)))))))
	(elem-set rec 0 them))

(defun storage-recording? (rec)
	; (storage-recording? rec) -> :nil | :t
	;is a recorder told, those whose time is up are forgotten
	(defq now (pii-time) them (first rec))
	(if (some (# (>= now (second %0))) them)
		(elem-set rec 0 (setq them (filter (# (< now (second %0))) them))))
	(nempty? them))

(defun storage-record (rec member kind &optional what)
	; (storage-record rec member kind [what]) -> rec
	;a thing done, kept to be sent, if there is a recorder
	(if (storage-recording? rec)
		(push (second rec) (cat (str (pii-time)) " " member " " kind (if what (cat " " what) ""))))
	rec)

(defun storage-record-send (rec)
	; (storage-record-send rec) -> rec
	;what was kept is sent, in one mail
	(when (nempty? (second rec))
		(when (storage-recording? rec)
			(defq text (join (second rec) +storage_lf))
			(each (# (mail-send (first %0) text)) (first rec)))
		(elem-set rec 1 (list)))
	rec)

(defun storage-record-name (hash)
	;a block as a record names it, enough of it to be told from the rest,
	;- for none, a path deleted
	(if (< (length hash) 8) "-" (hex-encode (slice hash 0 8))))

(defun storage-record-ask (svc mbox &optional stop)
	; (storage-record-ask svc mbox [stop]) -> :nil | :t
	;a member is to tell that mailbox what it does, for so long, or with
	;stop, not to. With :nil for the mailbox, it is to tell nobody
	(if (storage-ask svc +storage_type_record
		(cond ((not mbox) "-") (stop (cat "-" (hex-encode mbox))) ((hex-encode mbox)))) :t))

(defun storage-record-members (&optional group)
	; (storage-record-members [group]) -> (netid ...)
	;the services of every member of a store of that group that is there,
	;on every machine, *Storage if not given
	(map (const mail-entry-id) (mail-enquire (cat (ifn group +storage_group) ","))))

(defun storage-record-run (write until &optional group stop)
	; (storage-record-run write until [group stop]) -> lines
	;what every member of a group does, each line given to write as it
	;comes, till the time is until, (pii-time), or a mail is on the stop
	;mailbox. Each member is asked again each second, and one that has
	;come since is asked then too. write is given :nil when nothing has
	;come for a tenth of a second, for one that holds lines back to send a
	;few at a time. How many lines there were. Its names
	;are its own, record_..., so that write finds its own
	(defq record_mbox (mail-mbox) record_next 0 record_lines 0)
	(while (and (< (pii-time) until) (not (and stop (mail-poll (list stop)))))
		(when (>= (pii-time) record_next)
			(each (# (storage-send %0 +storage_type_record (hex-encode record_mbox))) (storage-record-members group))
			(setq record_next (+ (pii-time) 1000000)))
		(if (defq record_text (mail-read-timeout record_mbox 100000))
			(each (lambda (line) (++ record_lines) (write line)) (split record_text +storage_lf))
			(write :nil)))
	;they are told to stop telling this one, and what comes after is to a
	;mailbox nobody reads
	(each (# (storage-send %0 +storage_type_record (cat "-" (hex-encode record_mbox)))) (storage-record-members group))
	record_lines)

(defun storage-record-seed (write &optional group)
	; (storage-record-seed write [group]) -> lines
	;what every member of a group has now, as the lines a recording of it
	;from the start would have left, each given to write: who each sees,
	;what each has each path as, the blocks of each object, and which of
	;them each keeps. So a recording begun now has what came before. How
	;many lines there were. Its names are its own, seed_..., so that write
	;finds its own
	(defq seed_now (str (pii-time)) seed_objects (Fmap 101) seed_lines 0 seed_members (list))
	(defun seed-line (text) (++ seed_lines) (write (cat seed_now " " text)))
	(each (lambda (seed_svc)
		(when (defq seed_info (storage-info seed_svc))
			(defq seed_me (. seed_info :find :member))
			(push seed_members (list seed_me seed_svc))
			(each (lambda (line) (if (nempty? line) (seed-line (cat seed_me " peer+ " (first (split line " "))))))
				(split (ifn (storage-ask seed_svc +storage_type_peers) "") +storage_lf))
			(each (lambda (entry)
				(defq seed_m (elem-get entry +store_entry_hash))
				(unless (. seed_objects :find seed_m)
					(when (defq seed_got (storage-manifest (first entry) seed_svc))
						(. seed_objects :insert seed_m (second seed_got))
						(seed-line (cat seed_me " object " (storage-record-name seed_m) " "
							(join (map (const storage-record-name) (second seed_got)) " ")))))
				(seed-line (cat seed_me " learnt " (storage-record-name seed_m) " " (elem-get entry +store_entry_writer)
					" " (first entry))))
				(ifn (storage-list "" seed_svc) (list)))))
		(storage-record-members group))
	;the blocks each keeps, of all those known, asked a few hundred at a time
	(defq seed_names (list))
	(. seed_objects :each (lambda (m names)
		(push seed_names m)
		(each (# (push seed_names %0)) names)))
	(each (lambda ((seed_me seed_svc))
		(each (lambda (at)
			(defq seed_some (slice seed_names at (min (length seed_names) (+ at 256)))
				seed_has (ifn (storage-ask seed_svc +storage_type_has (apply (const cat) (cat (list "") seed_some))) ""))
			(each (lambda (name)
				(and (< (!) (length seed_has)) (eql (elem-get seed_has (!)) "1")
					(seed-line (cat seed_me " block+ " (storage-record-name name)))))
				seed_some))
			(range 0 (length seed_names) 256)))
		seed_members)
	seed_lines)

;;;;;;;;;;;
; snapshots
;;;;;;;;;;;

(defun storage-snapshot (name &optional prefix svc)
	; (storage-snapshot name [prefix svc]) -> :nil | count
	;a snapshot of the objects whose paths begin so, all if not said, kept
	;as snapshots/name/ and their paths, how many: no block is copied, see
	;lib/storage/store.inc. :nil if the name is not one a snapshot may have,
	;not empty, no / or space
	(and (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_snapshot
			(cat name +storage_lf (ifn prefix ""))))
		(nql answer "") (str-as-num answer)))

(defun storage-snapshots (&optional svc)
	; (storage-snapshots [svc]) -> :nil | ((name count) ...)
	;the snapshots there are, and how many objects each is of
	(when (defq entries (storage-list +store_snapshots svc))
		(defq out (list) counts (Fmap 11))
		(each (lambda (entry)
			(defq after (slice (first entry) (length +store_snapshots) -1) name (slice after 0 (ifn (find "/" after) 0)))
			(when (nempty? name)
				(unless (. counts :find name) (push out name))
				(. counts :insert name (inc (ifn (. counts :find name) 0)))))
			entries)
		(map (# (list %0 (. counts :find %0))) (sort out))))

(defun storage-restore (name &optional prefix svc)
	; (storage-restore name [prefix svc]) -> :nil | (restored newer)
	;the objects whose paths begin so are as they were in the snapshot,
	;all of it if not said, how many were set back, and how many paths
	;there are now under the prefix that the snapshot has not, which are
	;left. :nil if there is no such snapshot
	(and (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_restore
			(cat name +storage_lf (ifn prefix ""))))
		(nql answer "") (map (const str-as-num) (split answer " "))))

(defun storage-resized-name (image megabytes)
	; (storage-resized-name image megabytes) -> path
	;where a store of that file is put when it is made that size: beside
	;it, the size in the name, storage/store.8192MB.img
	(defq base (if (ends-with ".img" image) (slice image 0 -5) image)
		at (find "." (reverse base)))
	(and at (ends-with "MB" base) (> at 2)
		(some! (# (find %0 "0123456789")) (list base) :t (- (length base) at) -3)
		(setq base (slice base 0 (- (length base) at 1))))
	(cat base "." (str megabytes) "MB.img"))

(defun storage-resize (megabytes &optional svc)
	; (storage-resize megabytes [svc]) -> :nil | path
	;the store of this member put on a volume of that many MB, a copy on
	;this machine, and the file it is in now: storage/store.8192MB.img for
	;storage/store.img. A machine whose member it is starts it from there
	;from now on. :nil if it is that size already, or can not be made so,
	;and it is as it was. The member answers nothing while it copies
	(if (nempty? (defq answer (ifn (storage-ask (ifn svc (storage-service)) +storage_type_resize
			(str megabytes) 3600) ""))) answer))

(defun storage-snapshot-drop (name &optional svc)
	; (storage-snapshot-drop name [svc]) -> :nil | count
	;a snapshot is gone, how many paths it had. Its blocks are let go of by
	;a sweep, if nothing else is made of them
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_snapshot_drop name))
		(str-as-num answer)))

