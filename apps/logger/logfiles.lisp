;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logfiles- Log file management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/pathnode/pathnode.inc")

(defq
  +LOGPATH+       "/logs"
  +LOGEXT+        ".log"
  _logs_path      nil
  _log_registry   (xmap))

; _log_register
; :handler_key
;   (emap
;     :prefix string
;     :logs   sorted list of (logname (pii-fstat array)))

(when (nil? _logs_path)
  (setq _logs_path (node-for (str (gets-enval "ROOT") +LOGPATH+))))

(defun _prefix-filter (lprefix entry)
  ; (_prefix-filter fname-prefix value-to-check)
  ; Matching filter
  (starts-with lprefix (first entry)))

(defun _sort-age (l r)
  ; (_sort-age el el) -> -1, 0, 1
  ; Comparator for ordering the file by age in ascending order
  (if (> (first (second r)) (first (second l))) -1 0))

(defun _register-logs (lkey lprefix)
  ; (_register_logs)
  (unless (gets _log_registry lkey)
    (sets!
      _log_registry
      lkey
      (emap-kv
        :prefix lprefix
        :logs (sort _sort-age (reduce
             (lambda (acc _k)
               (defq fqn (. _logs_path :fqname _k))
               (push acc (list fqn (pii-fstat fqn))))
             (. _logs_path :all_members
                _pn-name-only (curry _prefix-filter lprefix))
             (list))))))
  (gets _log_registry lkey))

(defun _genlog_name (lprefix)
  (str (. _logs_path :fqname lprefix) "-" (pii-time) +LOGEXT+))

(defun _genlog_new (lprefix llist)
  (defq
    lname (_genlog_name lprefix)
    flog  (file-stream lname file_open_append))
  (push llist (list lname (pii-fstat lname)))
  flog)

(defun _remove-top-log (llist)
  ; (_remove-top-log loglist)
  (pii-remove (first (first logs)))
  (slice 1 -1 llist))

(defun open-latest-log (lkey lprefix max_count max_size)
  ; (open-latest-log key prefix max_count max_size) -> file-stream
  ; Get all logs for prefix
  (defq
    reg   (_register-logs lkey lprefix)
    logs  (gets reg :logs)
    fstrm nil)
  (cond
    ; Create log if empty
    ((empty? logs)
     (setq fstrm (_genlog_new lprefix logs)))
    ; If last one is maxed out
    ((> (second (second (last logs))) max_size)
     (setq fstrm (_genlog_new lprefix logs)))
    ; Open recent
    (t
      (setq fstrm (file-stream (first (last logs)) file_open_append))))

  ; Check max files to keep
  (when (> (length logs) max_count)
    (sets! reg :logs (_remove-top-log logs)))

  (list fstrm (second (second (last logs)))))

