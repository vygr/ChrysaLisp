;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logservice - Logging Service
; Log Service - a flexible log event
; system for commands, applicaations and
; libraries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

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
  (netid 'receiver+)
  (offset 'data+))

; Events for mail_msg_commands
(structure '+log_event 0
  (byte
    'register_anchor+
    'register_anchor_activate+
    'register_anchor_with_configuration+
    'query_anchor_config+
    'drop_anchor+
    'anchor_info+
    'logsetcfg+
    'logmsg+
    'register+
    'deregister+
    'shutdown+
    'registered+
    'errored+
    'ping+))

(defun get-logsrvc-mb ()
  ; (get-logsrvc-mb) -> mailbox id | nil
  ; Retrieves the service mailbox for logging
  ; If not found in service registry, start
  ; the service. Otherwise return the mailbox id
  (cond
    ((nil? log_srvc_mbox_id)
      (defq res (mail-enquire +logging_srvc_name+))
      (setq log_srvc_mbox_id
        (if (nempty? res)
            (to-net-id (second (split (first res) ",")))
            (open-child "apps/logger/app.lisp" kn_call_open))))
    (t log_srvc_mbox_id)))


(defun to-kw (_)
  ; (to-kw el) -> keyword
  ; Converts a string or symbol to a keyword
  ; example:
  ; (kw "foo") -> :foo
  ; (kw ":foo") -> :foo
  (if (or (str? _) (sym? _))
    (if (eql (first _) ":")
      (sym _)
      (sym (str ":" _)))
    (throw "Expected symbol or string, found " _)))

(defun release-log-anchor ()
  ; (release-log-anchor keyword) -> nil
  ; Drops the logging anchor in the server
  (if (defq ankw (get '*log_anchor* (penv (env))))
    (progn
      (mail-send
        (get-logsrvc-mb)
        (cat (char +log_event_drop_anchor+ long_size) (str ankw)))
      (undef (penv (env)) '*log_anchor*))))

(defun create-log-anchor (nm henv &rest override_pairs)
  ; (create-log-anchor name environment [override_pairs])
  ; Registration for anchor loggers in environment

  ; Setup the unique anchor key
  (defq astr (to-kw (str nm "_" (hash henv))))
  ; Register this anchor key in environment
  (def henv '*log_anchor* astr)

  ; Full handler configuration for passing if not registered
  (defq
      sstrm   (string-stream (cat ""))
      mbx_id  (mail-alloc-mbox)
      full_cnfg  (list
        :key_name   astr
        :name       nm
        :type       :file
        :level      :info
        :formatter  :standard
        :file_name  nm
        :rotate     '*true
        :maxbytes   10485760
        :backups    10))

  (when (> (length override_pairs) 0)
    (each (#
      (when (defq kdx (find full_cnfg (first %0)))
        (elem-set (inc kdx) full_cnfg (second %0))))
      (partition 2 override_pairs)))
  ; Query state of anchor in service
  (mail-send
    (get-logsrvc-mb)
    (cat
      (char +log_event_query_anchor_config+ long_size)
      mbx_id
      (str nm)))
  (defq
    qmsg (mail-read mbx_id)
    qid  (get-long qmsg mail_msg_command)
    pay  (slice mail_msg_data -1 qmsg)
    cmd  nil
    opay nil)
  (bind '(reg hof) (split pay ","))
  ; Setup appropriate payload
  (cond
    ; Active anchor
    ((eql (first hof) ":")
      (defq
        ckw (to-kw hof)
        clst (list
          :name nm
          :handler ckw
          :reciever mbx_id))
      (set henv '*log_anchor* ckw)
      (setq
        cmd +log_event_register_anchor+
        opay (str clst)))
    ; Registered but no active anchor
    ((eql (first reg) "t")
      (defq
        alst (list
          :name nm
          :handler astr
          :reciever mbx_id))
      (setq
        cmd +log_event_register_anchor_activate+
        opay (str alst)))
    ; Not registered, register and open logger
    (t
      (setq
        cmd +log_event_register_anchor_with_configuration+
        opay (str full_cnfg))))

  (write sstrm (str "{" (slice 1 -2 opay) "}"))
  (mail-send
    (get-logsrvc-mb)
    (cat
      (char cmd  long_size)
      mbx_id
      (str sstrm)))
  (defq
    rmsg (mail-read mbx_id)
    rid  (get-long rmsg mail_msg_command))
  ; Return the anchor logger or instance of anchor logger
  (slice mail_msg_data -1 rmsg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For debugging only. Typically the service
; just keeps on running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shutdown-log-service ()
  (mail-send (get-logsrvc-mb) (char +log_event_shutdown+ long_size))
  (setq log_srvc_mbox_id nil))

(defun ping-service ()
  (mail-send
    (get-logsrvc-mb)
    (cat
      (char +log_event_ping+ long_size)
      (task-mailbox))))
