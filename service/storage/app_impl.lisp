;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, the service
;
; It keeps one store, lib/storage/store.inc, and does what it is asked of
; it by mail, service/storage/app.inc. One of a name for a system, @Storage
; if it is not given another.
;
; The first mail it has says what it is to keep: who to tell when it is
; ready, its name, the file of the host the store is in, and, if there may
; be none there yet, how big to make one, the store's id, its block
; size, and whether what is there and is no store is to be made one.
;
; It does each thing asked before it looks at the next, so a write is all
; done or not begun as far as any other asker can see. A gopher is not run
; here, it is a task of its own, ./gopher.lisp, that asks this one for the
; blocks: what it does and how long it takes are its own affair.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")

(defun names-of (text)
	;names sent one after another, 32 bytes each
	(map (# (slice text %0 (+ %0 32))) (range 0 (length text) 32)))

(defun said (result)
	;what a write came to, as it is sent back
	(bind '(what val) result)
	(case what
		((:ok :stale) (cat (if (eql what :ok) "ok" "stale") +storage_lf (if val (store-entry-line val) "")))
		(:missing (join (cat (list "missing") (map (const hex-encode) val)) +storage_lf))
		(:full "full")
		(:t "none")))

(defun from-of (text)
	;what a path was said to be: ? not said, - none
	(cond ((eql text "?") :nil) ((eql text "-") "") ((hex-decode text))))

(defun info-text (store)
	(bind '(objects blocks free) (store-info store))
	(join (list (cat "id " (get :id store)) (cat "member " (get :member store))
		(cat "members " (join (get :members store) " "))
		(cat "block_size " (str (get :block_size store))) (cat "copies " (str (get :copies store)))
		(cat "objects " (str objects)) (cat "blocks " (str blocks)) (cat "free " (str free)))
		+storage_lf))

(defun main ()
	(bind '(ready name image size id block_size force) (split (mail-read (task-mbox)) +storage_lf))
	(defq ready (hex-decode ready) size (str-as-num size) block_size (str-as-num block_size)
		member (hex-encode (system-id))
		;the store that is there
		store (if (/= (age image) 0) (store-open image member)))
	;or a new one, where there is nothing. Where there is something that
	;is no store, only if it was said to
	(and (not store) (> size 0) (or (= (age image) 0) (eql force "1"))
		;the volume has the store's id as its label, as much of it as a label holds
		(store-make image size id member (if (> block_size 0) block_size) (slice id 0 (min 11 (length id))))
		(setq store (store-open image member)))
	(cond
		((or (not store) (nempty? (mail-enquire (cat name ","))))
			(mail-send ready "no"))
		(:t (defq service (mail-declare (task-mbox) name "Storage Service 0.1") running :t)
			(mail-send ready "ok")
			(while running
				(defq msg (mail-read (task-mbox)) reply (getf msg +storage_rpc_reply_id)
					type (getf msg +storage_rpc_type) body (slice msg +storage_rpc_size -1))
				;a thing asked that throws is said to have, and the service goes on
				(defq answer (catch (case type
						(+storage_type_info (info-text store))
						(+storage_type_stat (if (defq entry (store-stat store body)) (store-entry-line entry) ""))
						(+storage_type_list
							(join (map (const store-entry-line) (store-list store body)) +storage_lf))
						(+storage_type_has
							(apply (const cat) (cat (list "")
								(map (# (if (store-block-has? store %0) "1" "0")) (names-of body)))))
						(+storage_type_block_put (ifn (store-block-put store body) ""))
						(+storage_type_block_get (ifn (store-block-get store body) ""))
						(+storage_type_commit
							(bind '(from bytes path &rest names) (split body +storage_lf))
							(said (store-commit store path (map (const hex-decode) names) (str-as-num bytes) (from-of from))))
						(+storage_type_delete
							(defq at (find +storage_lf body))
							(said (store-delete store (slice body (inc at) -1) (from-of (slice body 0 at)))))
						(+storage_type_pin
							(store-pin store (rest body) (eql (first body) "-"))
							(join (get :pins store) +storage_lf))
						(+storage_type_pins_missing (join (store-pins-missing store) +storage_lf))
						(+storage_type_sweep (str (store-sweep store)))
						(+storage_type_check
							(join (map (const hex-encode) (store-check store (eql body "mend"))) +storage_lf))
						(+storage_type_walk
							;a gopher, a task of its own on this node. It is told who
							;asked, and answers them itself
							(defq gopher (open-child "service/storage/gopher.lisp" +kn_call_pin))
							(if (/= 0 (get-long gopher 0))
								(mail-send gopher (cat (hex-encode reply) +storage_lf (hex-encode (task-mbox)) +storage_lf body)))
							:nil)
						(+storage_type_stop (setq running :nil) "ok")
						(:t "error"))
					(progn (prin "Storage: " (slice (str _) 0 200)) (print) "error")))
				(if answer (mail-send reply answer)))
			(mail-forget service)
			(store-close store))))
