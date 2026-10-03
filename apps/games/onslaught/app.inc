;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught remote play RPC calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; any task, on any node, can play a running game via its "@Onslaught"
; service mailbox. keys sets the held control keys, a mask of the
; +fkey_xxx bits in "./enums.inc", state reports what is going on.

(enums +ons_type 0
	(enum keys state quit land))

(structure +ons_rpc 0
	(netid reply_id)
	(uint type keys))

(defun onslaught-service ()
	; (onslaught-service) -> netid | :nil
	(when (nempty? (defq s (mail-enquire "@Onslaught,")))
		(hex-decode (second (split (first s) ",")))))

(defun onslaught-rpc (type &optional keys)
	; (onslaught-rpc type [keys]) -> reply | :nil
	(defq svc (onslaught-service) mbox (mail-mbox))
	(and svc
		(mail-send svc (setf-> (str-alloc +ons_rpc_size)
			(+ons_rpc_reply_id mbox)
			(+ons_rpc_type type)
			(+ons_rpc_keys (ifn keys 0))))
		(mail-read-timeout mbox (task-timeout 2))))

(defun onslaught-keys-rpc (keys)
	; (onslaught-keys-rpc keys) -> :t | :nil
	(if (onslaught-rpc +ons_type_keys keys) :t :nil))

(defun onslaught-state-rpc ()
	; (onslaught-state-rpc) -> lmap | :nil
	(when (defq reply (onslaught-rpc +ons_type_state))
		(reduce (lambda (state (k v)) (. state :insert k v))
			(partition (first (read (string-stream reply))) 2) (Lmap))))

(defun onslaught-land-rpc ()
	; (onslaught-land-rpc) -> str | :nil
	;the +fmap_xxx flags of the battle map, one char per tile, row by row
	(onslaught-rpc +ons_type_land))

(defun onslaught-quit-rpc ()
	; (onslaught-quit-rpc) -> :t | :nil
	(if (onslaught-rpc +ons_type_quit) :t :nil))
