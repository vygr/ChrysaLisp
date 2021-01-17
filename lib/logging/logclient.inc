;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; log_client - Logging client IPC library
; Wrapper for interchanging requests with the
; logging service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/client_ipc.inc")

(defq
  _log_service_mb nil
  _anchor_kwy     nil)

(defun logsrvc-mb (&optional node)
  ; (logsrvc-mb) -> mailbox id | nil
  ; Retrieves the service mailbox for the logging
  ; service.
  ; If not found in service registry, start
  ; the service. Otherwise return the mailbox id
  (cond
    ((nil? _log_service_mb)
      (setd node (remote-node))
      (defq
        res   (mail-enquire +logging_srvc_name+))
      (setq _log_service_mb
        (if (nempty? res)
            (to-net-id (second (split (first res) ",")))
            (open-remote
              "apps/logger/app2.lisp"
              node
              kn_call_open))))
    (t _log_service_mb)))

; log-client
(defclass log-client (&optional client server) (client-ipc client server)

  (defmethod :register_logger (this name &optional handler)
    ; (. log-client :register_logger name) -> result | exception
    (print "Registering logger " name)
    (.super this :register
      (xmap-kv
        :name     name
        :kind     :logger))
    )

  (defmethod :send_log_msg (this loglevel msg)
    )
  (defmethod :log_debug (this msg)
    )
  (defmethod :log_info (this msg)
    )
  (defmethod :log_info (this msg)
    )
  (defmethod :log_warning (this msg)
    )
  (defmethod :log_error (this msg)
    )
  (defmethod :log_critical (this msg)
    )
  (defmethod :shutdown (this)
    ; (. democ :shutdown) -> nil
    ; This is for demo only
    (print "Client shutdown sending")
    (bind '(client cmd msg)
      (.
        (. this :send :shutdown)
        :recieve))
    (setq _log_service_mb nil)
    (print "Shutdown result = " msg)
    this)

  (defmethod :ping (this)
    ; (. democ :ping) -> list
    (print "Client ping sending")
    (bind '(client cmd msg)
      (.
        (. this :send :ping)
        :recieve))
    (print "Ping result = " msg)
    this)
  )

; log-anchor
(defclass log-anchor (&optional client server) (log-client client server)

  (defmethod :register_anchor (this name)
    ; (. log-anchor :register  name) -> result | exception
    (print "Registering anchor " name)
    (.super this :register (xmap-kv :name name :kind :anchor))
    )
  )

; Ease of use functions

(defun logger (name)
  ; (logging-client name) -> log-client
  ; Default client
  ;   Uses anchor if it exists, else uses :applog
  (defq
    lipc (log-client (mail-alloc-mbox) (logsrvc-mb))
    qry  (xmap-kv
            :name name
            :using :applog))

  (bind '(client reskw msg)
    (. (. lipc :send :query_config qry) :recieve))
  (cond
    ; If anchored configured
    (_anchor_kwy
      (sets! qry :using _anchor_kwy))
    ; Else if existing handler
    ((defq hndl (gets msg :handler))
      (sets! qry :using hndl)))
  (print "Will configure for: " (gets qry :using))
  lipc)

(defun console-logger (name)
  )

(defun logging-anchor (name)
  (when _anchor_kwy
    (throw "Anchor logger already set " _anchor_kwy))
  (setq _anchor_kwy (sym (cat : name)))
  _anchor_kwy)

; For client side REPL Testing start Terminal with at least 2 CPU
; ./run_tui.sh 2

; To test co-resident (optimized) client/server
; >lisp lib/logging/logclient.inc
; (defq lipc (log-anchor (mail-alloc-mbox) (logsrvc-mb (local-node))))
; Threaded messages - Double entries are to see effect
; of registering something already registered, etc.
; Shutdown is for demo to tell the server to stop
; (.-> lipc :ping (:register_anchor "foo") (:register_anchor "bar") :deregister :deregister :shutdown)
; Clear this for retesting
; (setq _log_service_mb nil)

; To test different CPU (serializing data) client/server
; >lisp lib/logging/logclient.inc
; (defq lipc (log-client (mail-alloc-mbox) (logsrvc-mb (remote-node))))
; Threaded messages
; (.-> lipc :ping :register :register :deregister :deregister :shutdown)
; (setq _log_service_mb nil)

