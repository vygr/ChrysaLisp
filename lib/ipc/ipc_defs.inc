;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ipc_defs - ipc utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
  _local_node  (slice 8 -1 (task-mailbox))
  _found_nodes nil
  _local_only  nil)

(setq _found_nodes (mail-nodes))

; If only have 1 it may be valid if that's the
; true number of nodes created
(when (<= (length _found_nodes) 1)
  (defq
    rtry  t
    rcnt  1)
  ; But we search for others if this invocation is being
  ; performed immediatley after boot up
  (while (and rtry (<= (length _found_nodes) 1) (< rcnt 10))
    (task-sleep 500000)
    (setq _found_nodes (mail-nodes))
    ; exit conditions
    (cond
      ((> (length _found_nodes) 1)
        (setq rtry nil))
      (t
        (setq rcnt (inc rcnt)))))
  (when (= (length _found_nodes) 1)
    (setq _local_only t)))

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
  ; (local-node) -> current task-mailbox node
  _local_node)

(defun any-node ()
  ; (any-node) -> node
  (elem-get (random (length _found_nodes)) _found_nodes))

(defun remote-nodes ()
  ; (remote-nodes) -> list
  (defq mine (to-service-id _local_node))
  (filter (#(not (eql mine (to-service-id %0)))) _found_nodes))

(defun remote-node ()
  ; (remote-node) -> node
  (first (remote-nodes)))