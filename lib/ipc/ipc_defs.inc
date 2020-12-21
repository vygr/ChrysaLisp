;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ipc_defs - ipc structures and utility
; functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(structure 'ipc_event 0
  (byte
    'success
    'exception
    'register
    'deregister
    'shutdown
    'ping))

(defun sdir-mbox (_s)
  ; (sdir-mbox _s) -> net_id
  (to-net-id (second (split (if (list? _s) (first _s) _s) ","))))

(defun mbox-node-id (mbox)
  ; (mbox-to-node-id mbox) -> binary string
  (slice 8 -1 mbox))

(defun node=? (mboxa mboxb)
  ; (node=? mailbox mailbox) -> t | nil
  (eql (mbox-node-id mboxa) (mbox-node-id mboxb)))

(defq _remote_nodes
  (let
    ((res (list))
     (mid (to-service-id (mbox-node-id (task-mailbox)))))
    (each
      (#(unless (eql mid (to-service-id %0)) (push res %0)))
      (mail-nodes))
    res))
