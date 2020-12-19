;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ipcdefs - ipc structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import "lib/xchange/msg-data.inc")

(structure 'ipc_event 0
  (byte
    'register
    'deregister
    'shutdown
    'ping))

(defun sdir-mbox (_s)
  (to-net-id (second (split (if (list? _s) (first _s) _s) ","))))

(defun mbox-to-node-id (mbox)
  (slice 16 -1 mbox))

(defun node=? (mboxa mboxb)
  (eql (mbox-to-node-id mboxa) (mbox-to-node-id mboxb)))