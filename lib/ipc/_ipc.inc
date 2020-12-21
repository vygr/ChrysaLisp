;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ipc - interprocess communication library
; Wrapper to generalize cross process and
; service communications
;
; Optimizes messaging depending on residence
; between source and sinks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/ipc/ipcdefs.inc")
(import "lib/class/reflect.inc")

(defq _ipc_service nil)

(defun ipc-test ()
  ; (ipc-test) -> mailbox id | nil
  ; Retrieves the service mailbox for local ipc testing
  ; If not found in service registry, start
  ; the service. Otherwise return the mailbox id
  (cond
    ((nil? _ipc_service)
      (defq res (mail-enquire "IPC"))
      (setq _ipc_service
        (if (nempty? res)
            (to-net-id (second (split (first res) ",")))
            (open-child
              "lib/ipc/ipctest.lisp"
              kn_call_open))))
    (t _ipc_service)))

(defun remote-ipc-test (node)
  ; (remote-ipc-test) -> mailbox id | nil
  ; Retrieves the service mailbox for remote ipc testing
  ; If not found in service registry, start
  ; the service on the remote node. Otherwise return the mailbox id
  (cond
    ((nil? _ipc_service)
      (defq res (mail-enquire "IPC"))
      (setq _ipc_service
        (if (nempty? res)
            (to-net-id (second (split (first res) ",")))
            (open-remote
              "lib/ipc/ipctest.lisp"
              node
              kn_call_open))))
    (t _ipc_service)))

(defun _default_msg_handler (&rest data)
  ; (_default_msg_handler client command data) -> list
  ; Default handler just returns data tuple
  data)

; ipc is the baseline class that performs
; the heavy lifting of communication between
; two endpoints:
; source - considered the client mailbox for receiving messages
; sink - considered the server mailbox to send message to

(defclass ipc (source sink) nil
  (def this
       :source      source
       :sink        sink
       :optimized   (node=? source sink))

  (defmethod :get_source (this)
    ; (. ipc :get_source) -> mailbox
    (get :source this))

  (defmethod :get_sink (this)
    ; (. ipc :get_sink) -> mailbox
    (get :sink this))

  (defmethod :optimized? (this)
    ; (. ipc :optimized) -> mailbox
    (get :optimized this))

  (defmethod :send (this cmd &optional msg)
    ; (. ipc :send cmd [message-block]) -> encoded message
    (mail-send
      (. this :get_sink)
      (cond
        ((. this :optimized?)
          (cat
            (list (. this :get_source) cmd)
             (cond
              ((list? msg) msg)
              ((nil? msg) (list))
              (t (list msg)))))
      (t
        (str
          (data-xser
            (xmap-kv
              :command  cmd
              :client   (to-service-id (. this :get_source))
              :data     msg))))))
    this)

  (defmethod :recieve (this &optional handler_fn)
    ; (. ipc :receive [receipt handler]) -> result
    (setd handler_fn _default_msg_handler)
    (defq fmsg (mail-read (. this :get_source)))
    (cond
      ((and (. this :optimized?) (list? fmsg))
        (handler_fn (first fmsg) (second fmsg) (last fmsg)))
      ((and (not (. this :optimized?)) (str? fmsg))
        (defq mmap (data-xdeser (write (string-stream (cat "")) fmsg)))
        (handler_fn (gets mmap :client) (gets mmap :command) (gets mmap :data)))
      (t
        (throw "Receive type undefined " (type-of fmsg)))))
  )

(gen-class-pred ipc)

; client_ipc derives from ipc to provide
; registering itself as a ipc source to ipc sink

(defclass client-ipc (client server) (ipc client server)
  (def this
    :registered  nil)

  (defmethod :shutdown (this)
    ; (. client-ipc :shutdown) -> nil
    ; This is for demo only
    (bind '(client cmd msg)
      (.
        (. this :send ipc_event_shutdown (list))
        :recieve))
    (print ":shutdown response cmd " cmd " data " msg))

  (defmethod :ping (this)
    ; (. client-ipc :ping) -> list
    (bind '(client cmd msg)
      (.
        (. this :send ipc_event_ping)
        :recieve))
    (print ":ping cmd " cmd " data " msg)
    this)

  (defmethod :register (this)
    ; (. client_ipc :register) -> t | exception
    (bind '(client cmd msg)
      (.
        (. this :send ipc_event_register)
        :recieve))
    (print ":register response cmd " cmd " data " msg)
    (if (= cmd ipc_event_success)
      (set this :registered t)
      (throw "Unable to register client " msg))
    this)

  (defmethod :deregister (this)
    ; (. client_ipc :deregister) -> t | exception
    (bind '(client cmd msg)
      (.
        (. this :send ipc_event_deregister)
        :recieve))
    (print ":deregister response cmd " cmd " data " msg)
    (if (= cmd ipc_event_success)
      (set this :registered nil)
      (throw "Unable to deregister client " msg))
    this)
  )

(gen-class-pred client-ipc)

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

; For client side REPL Testing start Terminal with at least 2 CPU
; ./run_tui.sh 2

; To test co-resident (optimized) client/server
; >lisp lib/ipc/_ipc.inc
; (defq cipc (client-ipc (task-mailbox) (ipc-test)))
; Threaded messages - Double entries are to see effect
; of registering something already registered, etc.
; Shutdown is for demo to tell the server to stop
; (.-> cipc :ping :register :register :deregister :deregister :shutdown)
; Clear this for retesting
; (setq _ipc_service nil)

; To test different CPU (serializing data) client/server
; >lisp lib/ipc/_ipc.inc
; (defq cipc (client-ipc (task-mailbox) (remote-ipc-test (first _remote_nodes))))
; Threaded messages
; (.-> cipc :ping :register :register :deregister :deregister :shutdown)
