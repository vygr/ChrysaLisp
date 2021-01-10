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

(defun prefix-filter (lprefix entry)
  (starts-with lprefix (first entry)))

(defun _sort-age (l r)
  ; (_sort-age el el) -> -1, 0, 1
  ; Comparator for ordering the file by age in ascending order
  (if (> (first (second r)) (first (second l))) -1 0))

(defun _register_logs (lkey lprefix)
  (unless (gets _log_registry lkey)
    (defq
      filt  (curry prefix-filter lprefix)
      files (. _logs_path :all_members _pn-name-only filt))
    (defq
      logs (sort _sort-age (reduce
             (lambda (acc _k)
               (defq fqn (. _logs_path :fqname _k))
               (push acc (list fqn (pii-fstat fqn))))
             files (list)))
      )
    ;.(print "  Identified logs with prefix " lprefix " = " logs)
    (sets!
      _log_registry
      lkey
      (emap-kv :prefix lprefix :logs logs)))
  (gets _log_registry lkey))

(defun open-latest-log (lkey lprefix max_size)
  (defq reg (_register_logs lkey lprefix))
  )
