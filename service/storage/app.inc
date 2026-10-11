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

(enums +storage_type 0
	(enum info stat list has block_put block_get commit delete
		pin pins_missing sweep check walk stop))

(structure +storage_rpc 0
	(netid reply_id)
	(uint type))

(defq +storage_name "@Storage" +storage_wait 10 +storage_lf (ascii-char 10)
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

(defun storage-info (&optional svc)
	; (storage-info [svc]) -> :nil | emap
	;what the store is: :id :member :members :block_size :copies :objects
	;:blocks :free
	(when (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_info))
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

(defun storage-block (hash &optional svc)
	; (storage-block name [svc]) -> :nil | data
	;a block by its name, if the service has it. It is checked against
	;its name here too, what came may not be what was sent
	(and (defq data (storage-ask (ifn svc (storage-service)) +storage_type_block_get hash))
		(nempty? data) (eql (sha256 data) hash) data))

(defun storage-put (name data &optional from svc)
	; (storage-put path data [from svc]) -> (:ok entry) | (:stale entry) | (:full :nil) | (:lost :nil)
	;a path is set to an object of this data, provided it is now the
	;object from: its name, "" for none, and not given, not to say. :stale
	;with what it is if it is not that. Only the blocks the service has
	;not got are sent
	(setd svc (storage-service))
	(cond
		((not (defq info (storage-info svc))) (list :lost :nil))
		(:t (defq size (. info :find :block_size)
				blocks (map (# (slice data %0 (min (length data) (+ %0 size)))) (range 0 (length data) size))
				names (map (const sha256) blocks)
				has (ifn (storage-ask svc +storage_type_has (apply (const cat) (cat (list "") names))) "")
				full :nil)
			(each (lambda (block hash)
				(unless (or full (eql (if (< (!) (length has)) (elem-get has (!))) "1"))
					(if (nql (storage-ask svc +storage_type_block_put block) hash) (setq full :t))))
				blocks names)
			(if full (list :full :nil)
				(storage-result (storage-ask svc +storage_type_commit
					(join (cat (list (storage-from from) (str (length data)) name) (map (const hex-encode) names))
						+storage_lf)))))))

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

(defun storage-delete (name &optional from svc)
	; (storage-delete path [from svc]) -> (:ok entry) | (:stale entry) | (:none :nil) | (:lost :nil)
	;there is no object of that name from now, provided it is from
	(storage-result (storage-ask (ifn svc (storage-service)) +storage_type_delete
		(cat (storage-from from) +storage_lf name))))

(defun storage-walk (name state visit &optional at wait svc)
	; (storage-walk path state visit [at wait svc]) -> :nil | (state)
	; visit is '(lambda (state data at count) ... (list state next))
	;a gopher is sent to an object. It is a task of its own, that has
	;what it was given, state, and a function, visit, and goes to the
	;blocks of the object one at a time, the first, or at: the function is
	;given what it has, the block, which block it is and how many there
	;are, and gives what it has now and the block to go to next, :nil when
	;it is done. What it has then comes straight back here, in a list.
	;:nil if there is no such object, a block it went to could not be had,
	;the function threw, or nothing came back in the time, seconds.
	;
	;visit is the function written out, quoted, not a function: it is
	;sent as text, with what it has, and that is all there is of the
	;gopher, so it can be sent on to wherever the next block is. What it
	;has must print as it reads, numbers, strs, lists of them
	(defq mbox (mail-mbox) svc (ifn svc (storage-service)))
	(when svc
		(mail-send svc (cat (setf-> (str-alloc +storage_rpc_size)
				(+storage_rpc_reply_id mbox) (+storage_rpc_type +storage_type_walk))
			(str (list name (ifn at 0) state visit))))
		(and (defq answer (mail-read-timeout mbox (task-timeout (ifn wait +storage_wait))))
			(starts-with "(" answer)
			(first (read (string-stream answer))))))

(defun storage-pin (prefix &optional off svc)
	; (storage-pin prefix [off svc]) -> :nil | (prefix ...)
	;this system holds all of every object whose path begins so, or with
	;off no longer does. What is pinned now
	(if (defq answer (storage-ask (ifn svc (storage-service)) +storage_type_pin (cat (if off "-" "+") prefix)))
		(filter (const nempty?) (split answer +storage_lf))))

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

(defun storage-start (image &optional size id name block_size force)
	; (storage-start image [size id name block_size force]) -> :nil | netid
	;the service is started on this node, to keep the store that is in
	;that file of the host. If there is none there and a size is given,
	;one is made of that many bytes, the store id, and this system its
	;member. :nil if there is no store and none could be made. What is
	;there and is not a store is left as it is, unless force: then it is
	;made one, and what was there is gone, a disk that is to be a store.
	;If the service of that name is there already, it is that one
	(setd name +storage_name)
	(cond
		((storage-service name))
		(:t (defq said (mail-mbox) task (open-child "service/storage/app.lisp" +kn_call_pin))
			(when (/= 0 (get-long task 0))
				;no part of it is nothing, a line of nothing is not a line
				(mail-send task (join (list (hex-encode said) name image (str (ifn size 0))
					(if (and id (nempty? id)) id "store") (str (ifn block_size 0)) (if force "1" "0")) +storage_lf))
				(if (eql (mail-read-timeout said (task-timeout 30)) "ok") (storage-service name))))))

(defun storage-stop (&optional svc)
	; (storage-stop [svc]) -> :nil | :t
	;the service puts what it has on the volume and goes
	(if (storage-ask (ifn svc (storage-service)) +storage_type_stop) :t))
