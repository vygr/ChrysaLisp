;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ipctest -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/_ipc.inc")

(when (= (length (mail-enquire "IPC")) 0)
  (defq
    active    t
    entry     (mail-declare (task-mailbox) "IPC" "IPC TEST")
    sipc      (server-ipc (task-mailbox)))
    (while active
      (bind '(client cmd msg) (. sipc :read))
      (cond
        ; Ping event
        ((= cmd ipc_event_ping)
          (. sipc :client_ping client))
        ; Shutdown event
        ((= cmd ipc_event_shutdown)
          (setq active nil)
          (. sipc :shutdown client))
        ; Register event
        ((= cmd ipc_event_register)
         (. sipc :register_client client))
        ; Deregister event
        ((= cmd ipc_event_deregister)
         (. sipc :deregister_client client))))

  (mail-forget entry))

