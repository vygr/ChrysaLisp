;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logging - Logging Service Library for clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")
(import "lib/xchange/msg-data.inc")
(import "lib/date/date.inc")

(defun log-registration (mname use_handler &optional rcvmb)
  ; (log-registration name keyword) -> xmap
  ; Setups the logging registration and optional configuration
  (xmap-kv
    :name       mname
    :handler    use_handler
    :reciever   (opt rcvmb (mail-alloc-mbox))))

(defun log-level-index (cfg lvl)
  ; (log-level-index configuration level-key) -> number
  ; Gets the log level index for the lvl key from levels
  ; defaults to 1 (info) if invalid lvl given
  (opt (gets-in cfg :levels lvl)  1))

(defun log-level-base (cfg)
  ; (log-level-base configuration) -> level key
  ; Gets the log level base key from the configuration
  (gets-in cfg :log_lvl))

(defun log-level-string (cfg lvl)
  ; (log-level-string configuration level-key) -> string
  ; Gets the string for output representing the log level key
  (elem (log-level-index cfg lvl) (gets-in cfg :levels :logstrs)))

(defun log-write (sstrm &rest _)
  ; (log-write strean ....) -> stream
  ; Wrap timestamp and nl to '_' arguments
  (setq _ (insert (push _ +nl+) 0 (list (encode-date))))
  (write sstrm (apply str _))
  (stream-flush sstrm))

(defun deser-inbound (msg)
  ; (deser-inbound mail-msg) -> collection
  ; Deserializes inbound data from mail message
  (data-xdeser (write (string-stream (cat "")) (slice mail_msg_data -1 msg))))

