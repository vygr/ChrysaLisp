;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The sync service. A machine that runs it takes a sync from another,
; and one that does not, does not. It is started by sync -a, and is
; sent its name and the root it may write under. It says the top of
; the tree of hashes of what is there, and what is in a folder of it,
; when asked, and writes and removes files there as it is told.
;
; Not by everyone: a key shared with a guest, another's machine in the
; mesh, lets it mail anything. Who it takes from is decided once for each
; machine that tells it, as it says who it is, (sync-from), see lib/sync/
; sync.inc, Who takes a sync: itself and those named in sync_from, or by
; the herd, at waypoints. Told by any other, it does nothing.
;
; And it spreads the herd's: its child, ./spread.lisp, offers this tree's
; waypoint to the others, as it starts, when it has taken one, when a
; machine that takes a sync is new to it, and when told by this machine,
; the waypoint command. No machine is in charge of it, each offers what
; it is at and takes what is newer, and a waypoint goes round them all.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/sync/sync.inc")

(defun main ()
	;its name, the root it writes under, and machines to take from as well
	;as those in sync_from, a system id each, with a space between: what
	;started it is this machine's own, a test starts one that takes from
	;the machine it is on
	(bind '(name root &optional also) (split (mail-read (task-mbox)) (ascii-char 10)))
	(defq also (filter (# (= (length %0) 32)) (split (ifn also "") " ")))
	;one for a machine. The name is a * one, seen from every machine, so it
	;is this machine's own that is looked for, by its system id. It is here
	;and not in app.lisp, as the other services have it, the name is not
	;known till the message is read
	(defq me (hex-encode (system-id)))
	(unless (some (# (eql (second %0) me)) (sync-services name))
	(defq service (mail-declare (task-mbox) name
			(cat "Sync Service 0.1 " (cpu) "/" (os) " " root))
		;the hashes of the system's own tree are kept, with what else is built
		kept (if (eql root ".") (cat "obj/" (cpu) "/" (abi) "/sync_hashes"))
		;the tree of hashes it last worked out, to be asked of folder by
		;folder. It is not kept once anything is written
		held :nil
		;the hash of each file, held from one asking to the next
		live (Fmap 101)
		out :nil out_path "" out_at 0 running :t real (Fset 31)
		;what each node that tells it may do, as it said who it is, and the
		;waypoint each said it was at
		told (Fmap 31) said_at (Fmap 31)
		;the child that offers this tree's waypoint, and the machines that
		;take a sync it has seen, by mailbox
		spread (open-child "service/sync/spread.lisp" +kn_call_pin)
		seen (Fset 31) looked (- (pii-time) +sync_look))
	(mail-send spread (cat name (ascii-char 10) root))
	;the root is the system's own tree, or a folder in it. Nowhere else, and
	;not by way of a link, whatever it was started with. If it is not, the
	;service takes nothing
	(defq fenced (or (eql root ".") (and (sync-safe? root) (sync-inside? "." (cat root "/x")))))
	(while running
		;a service that is new, and this one as it starts, is offered it,
		;looked for when there has been a while to. New by its mailbox, not
		;its machine: a machine come back, its node started again, is new
		(when (>= (- (pii-time) looked) +sync_look)
			(setq looked (pii-time))
			(defq now (Fset 31) new :nil)
			(each (# (. now :insert (defq m (hex-encode (first %0))))
					(unless (. seen :find m) (setq new :t)))
				(sync-services name))
			(setq seen now)
			(if new (mail-send spread "go")))
		(defq msg (mail-read-timeout (task-mbox) +sync_look))
		(when (and msg (>= (length msg) +sync_rpc_size))
			(defq reply_id (getf msg +sync_rpc_reply_id) kind (getf msg +sync_rpc_type)
				at (getf msg +sync_rpc_at) total (getf msg +sync_rpc_total)
				mode (logand (getf msg +sync_rpc_mode) 511)
				data (slice msg +sync_rpc_data -1) status 0 back ""
				node (slice reply_id +long_size -1))
			;who it is is said first, with a mode of 1 if only as the herd; one
			;from before that was said is taken as saying no waypoint. A yes is
			;kept, a no is not: a machine just linked may not yet be known to be
			;the one it is, and is asked about again next time
			(defq may (cond
				((= kind +sync_type_from)
					(. told :erase node) (. said_at :erase node)
					(when (nql (defq may (sync-from node data root kept me (cat (sync-named) also) (= mode 1))) :no)
						(. told :insert node may)
						(if (nempty? data) (. said_at :insert node data)))
					may)
				((. told :find node))
				((nql (defq may (sync-from node "" root kept me (cat (sync-named) also))) :no)
					(. told :insert node may) may)
				(may)))
			(catch
				(case (cond ((= kind +sync_type_quit) kind) ((eql may :no) -6) (fenced kind) (-1))
					(+sync_type_from)
					(+sync_type_waypoint
						;a push from one at a waypoint is done: if the tree is now
						;the same as the one at that waypoint, it is at it too, and
						;offers it on
						(setq held :nil)
						(when (defq said (. said_at :find node))
							(defq lines (map (# (trim %0)) (split said (ascii-char 10))))
							(cond
								((eql (sync-top root kept) (third lines))
									;one new to it is offered on
									(unless (eql (first lines) (first (sync-waypoint root)))
										(save said (cat root "/waypoint"))
										(mail-send spread "go")))
								((setq status -7)))))
					(+sync_type_spread
						;this machine's waypoint is noted, by one of its own
						(if (eql may :rig) (mail-send spread "go") (setq status -6)))
					(+sync_type_root
						;the rules are the sender's, and a mode of 1 is no modes
						(setq held (sync-tree root (sync-rules data) kept (= mode 1) live)
							back (hash-tree-root held)))
					(+sync_type_folder
						(if held (setq back (hash-tree-text (hash-tree-kids held data)))
							(setq status -5)))
					(+sync_type_put
						(setq held :nil)
						(defq nl (find (ascii-char 10) data)
							path (slice data 0 (ifn nl 0)) body (slice data (inc (ifn nl -1)) -1))
						(cond
							((not (and nl (sync-safe? path) (sync-inside? root path real))) (setq status -2))
							(:t ;a file starts at 0, and goes on from where it had got to
								(when (= at 0)
									(setq out (file-stream (cat root "/" path) +file_open_write)
										out_path path out_at 0))
								(cond
									((or (not out) (nql path out_path) (/= at out_at))
										(setq status -3 out :nil out_path ""))
									(:t (write-blk out body)
										(when (>= (setq out_at (+ out_at (length body))) total)
											(stream-flush out)
											(setq out :nil out_path "")
											;the file is whole, and is given its mode
											(if (/= mode 0) (pii-chmod (cat root "/" path) mode))))))))
					(+sync_type_del
						(setq held :nil)
						(if (and (sync-safe? data) (sync-inside? root data real))
							(pii-remove (cat root "/" data))
							(setq status -2)))
					(+sync_type_mode
						(setq held :nil)
						(if (and (/= mode 0) (sync-safe? data) (sync-inside? root data real)
								(= 0 (pii-chmod (cat root "/" data) mode)))
							:t (setq status -2)))
					(+sync_type_quit (setq running :nil))
					;from a machine it does not trust
					(-6 (setq status -6))
					(:t (setq status -1)))
				(progn (setq status -4 back (str _) out :nil out_path "") :t))
			(mail-send reply_id (cat (setf-> (str-alloc +sync_reply_size)
				(+sync_reply_status status)) back))))
	(mail-send spread "")
	(mail-forget service)))
