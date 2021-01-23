;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; server_ipc - Server IPC library
; Wrapper to generalize server ipc client
; registration functions and message retrieval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/ipc.inc")

; server-ipc is a wrapper
; for a server that maintains a registry of
; clients that have registered with it

(defclass server-ipc (server) nil
  (def this
    :server   server
    :registry (xmap))

  (defmethod :service_mb (this)
    ; (. service-ipc :service-mb) -> mailbox
    (get :server this))

  (defmethod :get_registry (this)
    (get :registry this))

  (defmethod :client_for (this client)
    ; (. server-ipc :client_for client) -> ipc | nil
    ; Returns a registered client ipc
    ; or nil if not registered yet
    (if (and (env? client) (ipc? client))
      client
      (gets (. this :get_registry) client)))

  (defmethod :register_client (this client &optional msg)
    ; (. server-ipc :register_client client msg) -> any
    ; Adds, if needed, client to registry and
    ; acknowledges a registration request from a
    ; client ipc
    (if msg
      (sets! msg :result "OK")
      (setq msg (xmap-kv :result "OK")))
    (defq
      reg (. this :client_for client))
    (if reg
      (sets! msg :result "Already Registered")
      (progn
        (setq reg (ipc (. this :service_mb) client))
        (sets! (. this :get_registry) client reg)))
    (. reg :send :success msg))

  (defmethod :deregister_client (this client)
    ; (. server-ipc :deregister_client client) -> any
    ; Removes, if needed, client from registry and
    ; acknowledges a request from a client ipc
    (setd msg (xmap-kv :result "OK"))
    (defq
      reg (. this :client_for client)
      txt "OK")
    (if reg
      (drop! (. this :get_registry) (. reg :get_sink))
      (progn
        (sets! msg :result "Not Registered")
        (setq reg (ipc (. this :service_mb) client))))
    (. reg :send :success msg))

  (defmethod :read_mail (this)
    ; (. server-ipc :read) -> triplet
    ; Reads messages from the server
    ; mailbox and prepares for delivery
    (defq
      _msg     (mail-read (. this :service_mb))
      _obj     (if (list? _msg)
                (first (xlist-data _msg))
                (first (xliststr-data _msg)))
      _client  (to-net-id (gets _obj :client)))
    (cond
      ((defq fc (. this :client_for _client))
        (setq _client fc))
      (t
        nil))
    (list _client (gets _obj :command) (gets _obj :data)))
  )
