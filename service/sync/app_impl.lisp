;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The sync service. A machine that runs it takes a sync from another,
; and one that does not, does not. It is started by sync -a, and is
; sent its name and the root it may write under. It lists that tree
; when asked, and writes and removes files there as it is told.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/sync/sync.inc")

(defun main ()
	(bind '(name root) (split (mail-read (task-mbox)) (ascii-char 10)))
	(defq service (mail-declare (task-mbox) name
			(cat "Sync Service 0.1 " (cpu) "/" (os) " " root))
		;the hashes of the system's own tree are kept, with what else is built
		kept (if (eql root ".") (cat "obj/" (cpu) "/" (abi) "/sync_hashes"))
		out :nil out_path "" out_at 0 running :t)
	(while running
		(defq msg (mail-read (task-mbox)))
		(when (>= (length msg) +sync_rpc_size)
			(defq reply_id (getf msg +sync_rpc_reply_id) kind (getf msg +sync_rpc_type)
				at (getf msg +sync_rpc_at) total (getf msg +sync_rpc_total)
				data (slice msg +sync_rpc_data -1) status 0 back "")
			(catch
				(case kind
					(+sync_type_list
						(setq back (sync-list-text (sync-list root (sync-rules data) kept))))
					(+sync_type_put
						(defq nl (find (ascii-char 10) data)
							path (slice data 0 (ifn nl 0)) body (slice data (inc (ifn nl -1)) -1))
						(cond
							((not (and nl (sync-safe? path))) (setq status -2))
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
											(setq out :nil out_path "")))))))
					(+sync_type_del
						(if (sync-safe? data)
							(pii-remove (cat root "/" data))
							(setq status -2)))
					(+sync_type_quit (setq running :nil))
					(:t (setq status -1)))
				(progn (setq status -4 back (str _) out :nil out_path "") :t))
			(mail-send reply_id (cat (setf-> (str-alloc +sync_reply_size)
				(+sync_reply_status status)) back))))
	(mail-forget service))
