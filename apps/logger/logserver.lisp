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

  (defmethod :server_send (this client cmd msg)
    (defq
      reg (. this :client_for client))
    (when (nil? reg)
      (setq reg (ipc (get :server this) client)))
    (. reg :send cmd msg))

  ; Constructor
  )

; "(:ast (:collection-kv :xmap (:collection-sequence :list (:keyword :command) (:keyword :query_config)) (:collection-sequence :list (:keyword :client) (:str "jaaaaaaaaaaaaaaapbhaopfofeepmjhknfclldkkkkaegnpa")) (:collection-sequence :list (:keyword :data) (:collection-kv :xmap (:collection-sequence :list (:keyword :name) (:str "repl"))))))"