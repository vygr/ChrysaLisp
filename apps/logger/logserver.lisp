;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logserver - Logging Service IPC server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/server_ipc.inc")

(defclass log-server (server) (server-ipc server)

  ; Methods
  (defmethod :client_ping (this client)
    (defq
      reg (. this :client_for client))
    (when (nil? reg)
      (setq reg (ipc (. this :service_mb) client)))
    (. reg :send :success "OK"))

  (defmethod :server_shutdown (this client)
    (defq
      reg (. this :client_for client))
    (when (nil? reg)
      (setq reg (ipc (get :server this) client)))
    (. reg :send :success "OK"))
  ; Constructor
  )