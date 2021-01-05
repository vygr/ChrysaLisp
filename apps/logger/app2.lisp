;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "apps/logger/logserver.lisp")
(import "lib/xtras/xtras.inc")
(import "lib/date/date.inc")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  (defq
    active    t
    sipc      (log-server (task-mailbox))
    entry     (mail-declare (task-mailbox) +logging_srvc_name+ "Logging Service 1.0")
    DEBUG     (file-stream "./logs/DEBUG_SERVICE.log" file_open_append))

  (defun debug-write (&rest _)
    (setq _ (insert (push _ +nl+) 0 (list (encode-date) " ")))
    (write DEBUG (apply str _))
    (stream-flush DEBUG))

  (while active
    (bind '(client cmd msg) (. sipc :read_mail))
    (cond
      ; Ping event
      ((eql cmd :ping)
        (. sipc :client_ping client))
      ; Shutdown event
      ((eql cmd :shutdown)
        (setq active nil)
        (. sipc :server_shutdown client))
      ; Register event
      ((eql cmd :register)
       (cond
         ((eql (gets msg :kind) :anchor)
          (debug-write "Anchor registration for " (gets msg :name)))
         ((eql (gets msg :kind) :logger)
          (debug-write "Logging registration for " (gets msg :name)))
         (t
           (throw "Unknown registration " (tolist msg))))
       (. sipc :register_client client))
      ; Deregister event
      ((eql cmd :deregister)
       (. sipc :deregister_client client))
       )
    )
  (mail-forget entry))
