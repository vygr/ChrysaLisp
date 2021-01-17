;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ipc_defs - ipc utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mbox-from-decl (_s)
  ; (mbox-from-decl _s) -> net_id
  (to-net-id (second (split (if (list? _s) (first _s) _s) ","))))

(defun mbox-for (sname)
  ; (mbox-for string) -> mailbox | nil
  (mbox-from-decl (mail-enquire sname)))

(defun mbox-node-id (mbox)
  ; (mbox-to-node-id mbox) -> binary string
  (slice 8 -1 mbox))

(defun node= (mboxa mboxb)
  ; (node= mailbox mailbox) -> t | nil
  (if (and mboxa mboxb)
    (eql (mbox-node-id mboxa) (mbox-node-id mboxb))))

(defun local-node ()
  (mbox-node-id (task-mailbox)))

(defun any-node ()
  (elem (random (length (defq nodes (mail-nodes)))) nodes))

(defun remote-nodes ()
  (defq mine (to-service-id (local-node)))
  (filter (#(not (eql mine (to-service-id %0)))) (mail-nodes)))

(defun remote-node ()
  (first (remote-nodes)))