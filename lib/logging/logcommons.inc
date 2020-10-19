;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logging - Logging Service Library for clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")
(import "lib/yaml-data/yaml-xchange.lisp")
(import "lib/date/date.inc")

(defq
  +logging_srvc_name+ "LOG_SERVICE")

; Mail message structure
; command - See +log_event+ below
; data - String block of data (yaml-xser'd)
(structure 'mail_msg 0
  (long 'command)
  (offset 'data))

; Events for mail_msg_commands
(structure '+log_event 0
  (byte
    'logmsg+
    'register+
    'deregister+
    'shutdown+
    'registered+
    'errored+))

(defun-bind log-level-index (cfg lvl)
  ; (log-level-index configuration level-key) -> number
  ; Gets the log level index for the lvl key from levels
  ; defaults to 1 (info) if invalid lvl given
  (defq res (getp-in cfg :levels lvl))
  (if res res  1))

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
  ; (deser-inbound mail-msg) -> properties
  ; Deserializes inbound data from mail message
  (yaml-xdeser (write (string-stream (cat "")) (slice mail_msg_data -1 msg))))

