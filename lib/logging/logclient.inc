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

  (defmethod :send_log_msg (this loglevel msg)
    (. this :send :logmsg
      (xmap-kv
        :text   (apply str msg)
        :level  loglevel)))

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
  )

; Ease of use functions
(defun log-debug (logr &rest msg)
  (. logr :send_log_msg :debug msg))

(defun log-info (logr &rest msg)
  (. logr :send_log_msg :info msg))

(defun log-warning (logr &rest msg)
  (. logr :send_log_msg :warning msg))

(defun log-error (logr &rest msg)
  (. logr :send_log_msg :error msg))

(defun log-critical (logr &rest msg)
  (. logr :send_log_msg :critical msg))

(defun logger (name)
  ; (logging-client name) -> log-client
  ; Default client
  ;   Uses anchor if it exists, else uses :applog
  (defq
    lipc (log-client (mail-alloc-mbox) (logsrvc-mb))
    ixch  (xmap-kv
            :name name
            :using :applog))

  (bind '(client reskw msg)
    (. (. lipc :send :query_config ixch) :recieve))
  (cond
    ((not (eql reskw :query_result))
      (throw "Query error " msg))
    ; If anchored configured
    (_anchor_kwy
      (sets! ixch :using _anchor_kwy))
    ; Else if existing handler
    ((defq hndl (gets msg :handler))
      (sets! ixch :using hndl)))
  (sets! ixch :kind :logger)
  (. lipc :register ixch)
  ; (print "Reg results " (entries (get :registered_data lipc)))
  lipc)

(defun console-logger (name)
  )

(defun logging-anchor (name)
  (when _anchor_kwy
    (throw "Anchor logger already set " _anchor_kwy))
  (defq
    lipc (log-anchor (mail-alloc-mbox) (logsrvc-mb))
    ixch  (xmap-kv
            :name name
            :using (cat : name)))


  (setq _anchor_kwy (sym (cat : name)))
  _anchor_kwy)

; For client side REPL Testing start Terminal with at least 2 CPU
; ./run_tui.sh 2
; >lisp lib/logging/logclient.inc
; (defq loga (logging-anchor "repl"))
; Setup anchor logger
; To test generic logger
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
; (defq lipc (logger "repl"))
; Threaded messages
; (.-> lipc :ping :register :register :deregister :deregister :shutdown)
; (setq _log_service_mb nil)

