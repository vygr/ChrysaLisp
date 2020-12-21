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

  (defmethod :client_for (this client)
    (if (ipc? client)
      client
      (gets (get :registry this) client)))

  (defmethod :register_client (this client)
    (defq
      reg (. this :client_for client)
      txt "OK")
    (if reg
      (setq txt "Already Registered")
      (progn
        (setq reg (ipc (get :server this) client))
        (sets! (get :registry this) client reg)))
    (. reg :send ipc_event_success txt))

  (defmethod :deregister_client (this client)
    (defq
      reg (. this :client_for client)
      txt "OK")
    (if reg
      (drop! (get :registry this) (. reg :get_sink))
      (setq
        txt "Not Registered"
        reg (ipc (get :server this) client)))
    (. reg :send ipc_event_success txt))

  (defmethod :read (this)
    (defq
      msg     (mail-read (get :server this))
      client  nil
      command nil
      data    nil)
    (cond
      ((list? msg)
       (setq
         client   (first msg)
         command  (second msg)
         data     (last msg)))
      (t
        (defq mmap
          (data-xdeser (write (string-stream (cat "")) msg)))
        (setq
          client  (to-net-id (gets mmap :client))
          command (gets mmap :command)
          data    (gets mmap :data))))
    (list (opt (. this :client_for client) client) command data))
  )
