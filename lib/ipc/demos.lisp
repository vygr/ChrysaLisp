;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; demos - Demonstration server that extends
; server-ipc for pinging and shutdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/server_ipc.inc")

(defclass demos (server) (server-ipc server)

   (defmethod :client_ping (this client)
    (defq
      reg (gets (. this :get_registry) client))
    (when (nil? reg)
      (setq reg (ipc (. this :service_mb) client)))
    ; (throw "server :ping " (ipc? reg))
    (. reg :send :success "OK"))

  (defmethod :server_shutdown (this client)
    (defq
      reg (gets (. this :get_registry) client))
    (when (nil? reg)
      (setq reg (ipc (get :server this) client)))
    (. reg :send :success "OK"))
  )

(when (= (length (mail-enquire "IPC")) 0)
  (defq
    active    t
    sipc      (demos (task-mailbox))
    entry     (mail-declare (task-mailbox) "IPC" "IPC TEST"))
    (while active
      (bind '(client cmd msg) (. sipc :read_mail))
      (cond
        ; Ping event
        ((eql cmd :ping)
          (. sipc :client_ping client))
        ; Shutdown event
        ((eql cmd :shutdown)
          (setq active nil)
          (. sipc :server_shutdown client))
        ; Register event
        ((eql cmd :register)
         (. sipc :register_client client))
        ; Deregister event
        ((eql cmd :deregister)
         (. sipc :deregister_client client))))

  (mail-forget entry))

