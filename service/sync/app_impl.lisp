;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The sync service. A machine that runs it takes a sync from another,
; and one that does not, does not. It is started by sync -a, and is
; sent its name and the root it may write under. It says the top of
; the tree of hashes of what is there, and what is in a folder of it,
; when asked, and writes and removes files there as it is told.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/sync/sync.inc")

(defun main ()
	(bind '(name root) (split (mail-read (task-mbox)) (ascii-char 10)))
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
		out :nil out_path "" out_at 0 running :t real (Fset 31))
	;the root is the system's own tree, or a folder in it. Nowhere else, and
	;not by way of a link, whatever it was started with. If it is not, the
	;service takes nothing
	(defq fenced (or (eql root ".") (and (sync-safe? root) (sync-inside? "." (cat root "/x")))))
	(while running
		(defq msg (mail-read (task-mbox)))
		(when (>= (length msg) +sync_rpc_size)
			(defq reply_id (getf msg +sync_rpc_reply_id) kind (getf msg +sync_rpc_type)
				at (getf msg +sync_rpc_at) total (getf msg +sync_rpc_total)
				mode (logand (getf msg +sync_rpc_mode) 511)
				data (slice msg +sync_rpc_data -1) status 0 back "")
			(catch
				(case (if (or fenced (= kind +sync_type_quit)) kind -1)
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
					(:t (setq status -1)))
				(progn (setq status -4 back (str _) out :nil out_path "") :t))
			(mail-send reply_id (cat (setf-> (str-alloc +sync_reply_size)
				(+sync_reply_status status)) back))))
	(mail-forget service)))
