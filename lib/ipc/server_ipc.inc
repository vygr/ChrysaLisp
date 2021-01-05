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
    ; (. server-ipc :register_client client) -> any
    ; Adds, if needed, client to registry and
    ; acknowledges a registration request from a
    ; client ipc
    (defq
      reg (. this :client_for client)
      msg (opt msg "OK"))
    (if reg
      (setq msg "Already Registered")
      (progn
        (setq reg (ipc (. this :service_mb) client))
        (sets! (. this :get_registry) client reg)))
    (. reg :send :success msg))

  (defmethod :deregister_client (this client)
    ; (. server-ipc :deregister_client client) -> any
    ; Removes, if needed, client from registry and
    ; acknowledges a request from a client ipc
    (defq
      reg (. this :client_for client)
      txt "OK")
    (if reg
      (drop! (. this :get_registry) (. reg :get_sink))
      (setq
        txt "Not Registered"
        reg (ipc (. this :service_mb) client)))
    (. reg :send :success txt))

  (defmethod :read_mail (this)
    ; (. server-ipc :read) -> triplet
    ; Reads messages from the server
    ; mailbox and prepares for delivery
    (defq
      msg     (mail-read (. this :service_mb))
      obj     (if (list? msg)
                (first (xlist-data msg))
                (first (xliststr-data msg)))
      client  (to-net-id (gets obj :client)))
    (cond
      ((defq fc (. this :client_for client))
        (setq client fc))
      (t
        nil))
    (defq
      res (list client (gets obj :command) (gets obj :data)))
    res)
  )

