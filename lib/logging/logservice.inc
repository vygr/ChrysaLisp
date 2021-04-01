;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logservice - Logging client IPC library
; Log Service - a flexible log event
; system for commands, applicaations and
; libraries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "lib/date/date.inc")
(import "lib/ipc/client_ipc.inc")

(defq
  +logging_srvc  "LOG_SERVICE"
  _log_service_mb nil
  _anchor_kword   nil
  clog    nil)

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
        res   (mail-enquire +logging_srvc))
      (setq _log_service_mb
        (if (nempty? res)
            (to-net-id (second (split (first res) ",")))
            (open-remote
              "apps/logger/app.lisp"
              node
              +kn_call_open))))
    (t _log_service_mb)))

; log-client
(defclass log-client (&optional client server) (client-ipc client server)

  (defmethod :send_log_msg (this loglevel msg)
    ; (. log-client :send_log_msg loglevel msg) -> log-client
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

; log-console
(defclass log-console (level) (log-client)
  (def this
    :level level)

  (defmethod :change_level (this level)
    (set this :level level))

  (defmethod :send_log_msg (this loglevel msg)
    (prin (apply str
           (insert
             (push msg +nl)
             0
             (list (encode-date) " [" loglevel "] ")))))

  (defmethod :shutdown (this)
    (. this :send_log_msg :error "Shutdown requires non-console logger"))

  (defmethod :ping (this)
    (. this :send_log_msg :error "Ping requires non-console logger"))
  )

; Allocate a console loggger

(when (nil? clog)
  (setq clog (log-console :info)))

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
  (when (not (eql reskw :add_result))
    (throw "Unable to add anchor configuration " (entries msg)))
  lipc)

(defun anchor-logger (name &optional lnode logspec)
  ; (anchor-logger name [logspec]) -> log-anchor
  (when _anchor_kword
    (throw "Anchor logger already set " _anchor_kword))
  (setq _anchor_kword (sym (cat : name)))
  (defq
    lipc (log-anchor (mail-alloc-mbox) (logsrvc-mb lnode))
    ixch  (xmap-kv
            :name name
            :kind :anchor
            :using (cat : name)))
  (bind '(client reskw msg)
    (. (. lipc :send :query_config ixch) :recieve))
  (defq hndkw (gets msg :handler))
  (cond
    ((not (eql reskw :query_result))
      (throw "Query error " msg))
    ; If handler is not known yet
    ((nil? hndkw)
      (add-anchor-spec lipc name logspec))
    (t t))
  ; I can now register the logger
  (. lipc :register
    (xmap-kv
      :name name
      :using _anchor_kword
      :kind  :anchor))
  lipc)

(defun logger (name &optional lnode)
  ; (logging-client name) -> log-client
  ; Default client
  ;   Uses anchor if it exists, else uses :applog
  (defq
    lipc (log-client (mail-alloc-mbox) (logsrvc-mb lnode))
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

(defun console-logger ()
  ; (console-logger) -> log-console
  clog)

; Regular logging
; (defq lipc (logger "repl"))
; (log-warning lipc "message"
; (. lipc :shutdown)
;
; Anchoring
; (defq lanch (anchor-logger "repl"))
; (log-info lanch "message"
; (. lanch :shutdown)
;
; Console logging
; (log-warning clog "This is a warning")

