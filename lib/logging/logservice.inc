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

; Registering the anchor
(structure '+rega_msg 0
  (long 'command+)
  (long 'receiver+)
  (offset 'data+))

; Events for mail_msg_commands
(structure '+log_event 0
  (byte
    'register_anchor+
    'register_anchor_with_configuration+
    'query_anchor_config+
    'anchor_info+
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
  ; (log-register-anchor name [override_pairs])
  ; Registration for anchors
  (def (penv (env)) '*log_anchor* nm)
  (defq
      sstrm   (string-stream (cat ""))
      mbx_id  (mail-alloc-mbox)
      full_cnfg  (list
        :key_name   nm
        :type       :file
        :level      :warning
        :formatter  :standard
        :file_name  nm
        :rotate     '*true
        :maxbytes   10485760
        :backups    10))

  (when (> (length override_pairs) 0)
    (each (#
      (when (defq kdx (find full_cnfg (first %0)))
        (elem-set (inc kdx) full_cnfg (second %0))))
      (partitioin 2 override_pairs)))
  ; Query state of anchor in service
  (mail-send
    (cat
      (char +log_event_query_anchor_config+ long_size)
      (char mbx_id long_size)
      (str nm))
    (get-logsrvc-mb))

  (defq
    qmsg (mail-read mbx_id)
    qid  (get-long qmsg mail_msg_command)
    res  nil
    reg  nil
    hof  nil
    cmd  nil)
  (print "Query service " qmsg)
  (if (= qid +log_event_anchor_info+)
    (setq
      res (split qmsg ",")
      reg (if (eql (first (first res)) "t") t)
      hof (if (eql (first (second res)) "t") t))
    (throw (str "Invalid return fro anchor query " qid) qmsg))
  ; Setup appropriate payload
  (cond
    (reg
      (print "Registered already")
      (setq cmd +log_event_register_anchor+)
      (write sstrm nm))
    (t
      (print "New registeration")
      (setq cmd +log_event_register_anchor_with_configuration+)
      (write sstrm (str "{" (slice 1 -2 (str full_cnfg)) "}"))))
  (mail-send
      (cat
          (char cmd  long_size)
          (char mbx_id long_size)
          (str sstrm))
      (get-logsrvc-mb))
  (defq
    rmsg (mail-read mbx_id)
    rid  (get-long rmsg mail_msg_command))
  (print "Registered " rmsg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For debugging only. Typically the service
; just keeps on running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-bind shutdown-log-service ()
  (mail-send (char +log_event_shutdown+ long_size) (get-logsrvc-mb))
  (setq log_srvc_mbox_id nil))

