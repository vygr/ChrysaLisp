;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logservice - Logging Service
; Log Service - a flexible log event
; system for commands, applicaations and
; libraries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
  +logging_srvc_name+   "LOG_SERVICE"
  log_srvc_mbox_id      nil)

; Mail message structure
; command - See +log_event+ below
; data - String block of data (string)
(structure 'mail_msg 0
  (long 'command)
  (offset 'data))

; Events for mail_msg_commands
(structure '+log_event 0
  (byte
    'register_anchor+
    'query_config+
    'config_info+
    'logsetcfg+
    'logmsg+
    'register+
    'deregister+
    'shutdown+
    'registered+
    'errored+))

(defun-bind get-logsrvc-mb ()
  ; (get-logsrvc-mb) -> mailbox id | nil
  ; Retrieves the service mailbox for logging
  ; If not found in service registry, start
  ; the service. Otherwise return the mailbox id
  (cond
    ((nil? log_srvc_mbox_id)
      (defq res (mail-enquire +logging_srvc_name+))
      (setq log_srvc_mbox_id
        (if (nempty? res)
            (str-to-num (second (split (first res) ",")))
            (open-child "apps/logger/app.lisp" kn_call_child))))
    (t log_srvc_mbox_id)))

; (push l0 :name "foo" :default :console :faux_num 12345)
; (write s0 (str "<" (slice 1 -2 (str l0)) ">"))
; (defq l0 (list))
; (mail-send (cat  string))

(defun-bind log-register-anchor (nm &rest override_pairs)
    (defq
        sstrm (string-stream (cat ""))
        cnfg  (list
          :key_name   nm
          :type       :file
          :level      :warning
          :formatter  :standard
          :file_name  t
          :rotate     nil
          :maxbytes   10485760
          :backups    10))

    (write sstrm (str "<" (slice 1 -2 (str cnfg)) ">"))
    (mail-send
        (cat
            (char +log_event_register_anchor+ long_size)
            (str sstrm))
        (get-logsrvc-mb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For debugging only. Typically the service
; just keeps on running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-bind shutdown-log-service ()
  (mail-send (char +log_event_shutdown+ long_size) (get-logsrvc-mb))
  (setq log_srvc_mbox_id nil))

