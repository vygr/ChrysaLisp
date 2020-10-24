;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logutils - ChrysaLisp Logging Service utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defq
  +logs_path+     "./logs/"
  +log_suffix+    ".log"
  +cfg_file+      "./apps/logger/logsrvc.yaml"
  +cfg_registry+  "./apps/logger/logregistry.yaml")

(defun make-log-filename (base)
  ; (make-log-filename basename) -> string
  ; Returns a fully qualified logfile path/name
  (str +logs_path+ base +log_suffix+))

(defun make-date-based-filename (base)
  ; (make-date-based-filename basename) -> string
  ; Returns a fully qualified, date bases, logfile path/name
  (make-log-filename base))

(defun open-log-file-stream (fname)
  (file-stream fname file_open_append))

(defun needs-rotation? (fh)
  ; (needs-rotation? properties) -> t | nil
  nil)

(defun rotate-logfile (fh)
  ; (rotate-logfile properties) -> properties
  fh)

(defun initialize-logfile-handler (cfg)
  ; (initialize-logfile-handler properties) -> properties
  ; Check
  ; Fully qualify name
  ; Ready new entries
  ; Open filestream
  (setp! cfg
    :handle (open-log-file-stream (make-log-filename (gets cfg :file_name))) t)
  ; Check for rotation
  (when (needs-rotation? cfg)
    (rotate-logfile cfg))
  cfg)

(defun create-log-file-handlers (handlers fsmap)
  ; (create-log-file-handlers properties kvmap) -> hmap
  (debug-write "Creating handlers!")
  ; Iterate through handlers looking for type :file
  ; For each, extend with file information and prepare
  ; for use
  (each (lambda (ent)
          (debug-write "each " ent)
          (cond
            ((eql (gets (second ent) :type) :file)
             (debug-write "handler setup-> " (first ent))
             (initialize-logfile-handler (second ent))
             (sets! fsmap (first ent) (second ent))
             (debug-write "added-> " (first ent)))
            (t nil)))
        (entries handlers))
  fsmap)

(defun-bind process-log-cfg ()
  ; (process-log-cfg) -> tuple
  ; Sets up logging configuration from YAML or fall back to bare bones
  ; Sets up logging file handlers as needed/specified
  (defq
    cfg_age   (age +cfg_file+)
    cfg       nil)
  (setq cfg
        (if (> cfg_age 0)
          (first (yaml-read +cfg_file+))
          (properties
            :logging (properties
              :levels (properties
                :debug     0
                :info      1
                :warning   2
                :error     3
                :critical  4
                :logstrs   (list "DEBUG" "INFO" "WARNING" "ERROR" "CRITICAL"))
             :formatters (properties
                :standard (properties
                  :format "tnlm"))
             :handlers (properties
                :console_handler (properties
                  :type       :stdout
                  :level      :info
                  :formatter  :standard)
                :system_handler (properties
                  :type       :file
                  :level      :info
                  :formatter  :standard
                  :file_name  "syslog"
                  :rotate     t
                  :maxbytes   10485760
                  :backups    10)
                :service_handler (properties
                  :type       :file
                  :level      :info
                  :formatter  :standard
                  :file_name  "logservice"
                  :rotate     t
                  :maxbytes   10485760
                  :backups    2))
             :loggers (properties
                :console (properties
                  :handler :console_handler)
                :service (properties
                  :handler :service_handler)
                :system (properties
                  :handler :system_handler))))))
  ; Build the system filesystem logger streams
  (defq fsmaps (hmap))
  (create-log-file-handlers (getp-in cfg :logging :handlers) fsmaps)
  (debug-write "fsmaps-> " fsmaps)
  (list
    (gets fsmaps :service_handler)
    (> cfg_age 0)
    cfg
    fsmaps))
