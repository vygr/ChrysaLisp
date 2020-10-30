;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logging - Logging Service Library for clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/hmap/hmap.inc")
(import "lib/xtras/xtras.inc")
(import "lib/yaml-data/yaml-xchange.lisp")
(import "lib/date/date.inc")

(defun-bind log-registration (mname use_handler &optional rcvmb)
  ; (log-registration name keyword) -> properties
  ; Setups the logging registration and optional configuration
  (sets-pairs! (properties)
    :name       mname
    :handler    use_handler
    :reciever   (opt rcvmb (mail-alloc-mbox))))

(defun-bind log-level-index (cfg lvl)
  ; (log-level-index configuration level-key) -> number
  ; Gets the log level index for the lvl key from levels
  ; defaults to 1 (info) if invalid lvl given
  (defq res (getp-in cfg :levels lvl))
  (opt res  1))

(defun-bind log-level-base (cfg)
  ; (log-level-base configuration) -> level key
  ; Gets the log level base key from the configuration
  (getp-in cfg :log_lvl))

(defun-bind log-level-string (cfg lvl)
  ; (log-level-string configuration level-key) -> string
  ; Gets the string for output representing the log level key
  (elem (log-level-index cfg lvl) (getp-in cfg :levels :logstrs)))

(defun-bind log-write (sstrm &rest _)
  ; (log-write strean ....) -> stream
  ; Wrap timestamp and nl to '_' arguments
  (setq _ (insert (push _ +nl+) 0 (list (encode-date))))
  (write sstrm (apply str _))
  (stream-flush sstrm))

(defun-bind deser-inbound (msg)
  ; (deser-inbound mail-msg) -> collection
  ; Deserializes inbound data from mail message
  (yaml-xdeser (write (string-stream (cat "")) (slice mail_msg_data -1 msg))))

