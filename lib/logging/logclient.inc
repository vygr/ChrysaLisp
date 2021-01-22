;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; log_client - Logging client IPC library
; Wrapper for interchanging requests with the
; logging service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/client_ipc.inc")

(defq
  _log_service_mb nil
  _anchor_kword     nil)

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
    ; (. log-client :shutdown) -> log-client
    ; This is for demo only
    (bind '(client cmd msg)
      (.
        (. this :send :shutdown)
        :recieve))
    (setq _log_service_mb nil)
    this)

  (defmethod :ping (this)
    ; (. log-client :ping) -> log-client
    (bind '(client cmd msg)
      (.
        (. this :send :ping)
        :recieve))
    (print "Ping result = " msg)
    this)
  )

; log-anchor
(defclass log-anchor (&optional client server) (log-client client server)

  (defmethod :shutdown (this)
    (setq _anchor_kword nil)
    (.super this :shutdown))
  )

; Ease of use log message functions
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

(defun add-anchor-spec (lipc name &optional logspec)
  ; (add-anchor-spec name [logspec]) -> nil
  ; Initiates a logging anchor, takes an optional
  ; logspec if not already configured on server or
  ; will use default:
  (when (nil? logspec)
    (defq
      hndlkw  (sym (cat _anchor_kword '_handler))
      _packet  (xmap-kv
        :name name
        :logger _anchor_kword
        :logspec nil)
      )
    (sets! _packet :logspec
      (xmap-kv
        :logging
        (xmap-kv
          :loggers
          (xmap-kv
            _anchor_kword (xmap-kv :handler hndlkw))
          :handlers
            (xmap-kv
              hndlkw (xmap-kv
                :type       :file
                :level      :info
                :formatter  :standard
                :file_name  name
                :rotate     t
                :maxbytes   10485760
                :backups    10))))))
  (bind '(client reskw msg)
    (. (. lipc :send :add_handler _packet) :recieve))
  (print "Result from adding anchor = " reskw " " (entries msg)))

(defun logging-anchor (name &optional logspec)
  ; (logging-anchor name [logspec]) -> log-anchor
  (when _anchor_kword
    (throw "Anchor logger already set " _anchor_kword))
  (setq _anchor_kword (sym (cat : name)))
  (defq
    lipc (log-anchor (mail-alloc-mbox) (logsrvc-mb))
    ixch  (xmap-kv
            :name name
            :kind :anchor
            :using (cat : name)))
  (bind '(client reskw msg)
    (. (. lipc :send :query_config ixch) :recieve))
  (print "Results of anchor query " (entries msg))
  (defq hndkw (gets msg :handler))
  (cond
    ((not (eql reskw :query_result))
      (throw "Query error " msg))
    ; If handler is not known yet
    ((nil? hndkw)
      (print "No match, adding configuration for " name)
      (add-anchor-spec lipc name logspec))
    (t t))
  ; I can now register the logger
  (. lipc :register
    (xmap-kv
      :name name
      :using _anchor_kword
      :kind  :anchor))
  lipc)

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
    (_anchor_kword
      (sets! ixch :using _anchor_kword))
    ; Else if existing handler
    ((defq hndl (gets msg :handler))
      (sets! ixch :using hndl)))
  (sets! ixch :kind :logger)
  (. lipc :register ixch)
  ; (print "Reg results " (entries (get :registered_data lipc)))
  lipc)

(defun console-logger (name)
  )

; Regular logging
; >lisp lib/logging/logclient.inc
; (defq lipc (logger "repl"))
; (log-warn lipc "message"
; (. lipc :shutdown)
;
; Anchoring
; >lisp lib/logging/logclient.inc
; (defq lanch (logging-anchor "repl"))
; (log-info lanch "message"
; (. lanch :shutdown)

