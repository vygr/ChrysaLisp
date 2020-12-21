;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; demos - Demonstration server that extends
; server-ipc for pinging and shutdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/server_ipc.inc")

(defclass demos (server) (server-ipc server)
   (defmethod :client_ping (this client)
    (defq
      reg (. this :client_for client))
    (when (nil? reg)
      (setq reg (ipc (get :server this) client)))
    ; (throw "server :ping " (ipc? reg))
    (. reg :send ipc_event_success "OK"))

  (defmethod :shutdown (this client)
    (defq
      reg (. this :client_for client))
    (when (nil? reg)
      (setq reg (ipc (get :server this) client)))
    (. reg :send ipc_event_success "OK"))
  )

(when (= (length (mail-enquire "IPC")) 0)
  (defq
    active    t
    entry     (mail-declare (task-mailbox) "IPC" "IPC TEST")
    sipc      (demos (task-mailbox)))
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

