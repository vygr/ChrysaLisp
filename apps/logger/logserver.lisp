;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logserver - Logging Service IPC server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/server_ipc.inc")

(defclass log-proxy (src_name log_name server_mb client_mb handler)
  (ipc server_mb client_mb)
  (def this
       :handler     handler
       :main_logger log_name
       :src_name    src_name)

  (defmethod :log (this msg)
    (. (get :handler this) :write (gets msg :level)
       (insert
         (list (gets msg :text))
         0
         (list (str (get :main_logger this) ":" (get :src_name this) " ")))))
  )

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

  (defmethod :server_send (this client cmd msg)
    (defq
      reg (. this :client_for client))
    (when (nil? reg)
      (setq reg (ipc (get :server this) client)))
    (. reg :send cmd msg))

  ; Overrides
  (defmethod :register_client (this client src_name log_name handler)
    ; (. log-server :register_client client name handler) -> any
    ; Registers a log-proxy for the client for log
    ; message handling

    (cond
      ((log-proxy? client)
        (. client :send :success (xmap-kv :result :registered)))
      (t
        (defq
          msg  (sets! (. levels :copy) :log_level (get :level handler))
          rc   (log-proxy src_name log_name (. this :service_mb) client handler))
        (sets! (. this :get_registry) client rc)
        (. rc :send :success msg))))

  (defmethod :deregister_client (this client)
    (.super this :deregister_client client))

  ; Constructor
  )
