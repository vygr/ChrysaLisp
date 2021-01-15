;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; log_client - Logging client IPC library
; Wrapper for interchanging requests with the
; logging service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/client_ipc.inc")

(defq
  _log_service_mb nil
  _anchor_name    nil)

(defun logsrvc-mb (node)
  ; (logsrvc-mb) -> mailbox id | nil
  ; Retrieves the service mailbox for the logging
  ; service.
  ; If not found in service registry, start
  ; the service. Otherwise return the mailbox id
  (cond
    ((nil? _log_service_mb)
      (defq res (mail-enquire +logging_srvc_name+))
      (setq _log_service_mb
        (if (nempty? res)
            (to-net-id (second (split (first res) ",")))
            (open-remote
              "apps/logger/app2.lisp"
              node
              kn_call_open))))
    (t _log_service_mb)))

; log-client
(defclass log-client (client server) (client-ipc client server)

  (defmethod :register_logger (this name)
    ; (. log-client :register_logger name) -> result | exception
    (print "Registering logger " name)
    (.super this :register (xmap-kv :name name :kind :logger))
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
        (. this :send :shutdown (list))
        :recieve))
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
(defclass log-anchor (client server) (log-client client server)

  (defmethod :register_anchor (this name)
    ; (. log-anchor :register  name) -> result | exception
    (print "Registering anchor " name)
    (.super this :register (xmap-kv :name name :kind :anchor))
    )
  )

(defun log (name)
  )

(defun log-anchor (name)
  )

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

