;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logutils - ChrysaLisp Logging Service utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defq
  +logs_path+ "./logs/"
  +log_suffix+ ".log"
  +cfg_file+   "./apps/logger/logsrvc.yaml")

(defun make-log-filename (base)
  ; (make-log-filename basename) -> string
  ; Returns a fully qualified logfile path/name
  (str +logs_path+ base +log_suffix+))

(defun make-date-based-filename (base)
  ; (make-date-based-filename basename) -> string
  ; Returns a fully qualified, date bases, logfile path/name
  (make-log-filename base))

(defun needs-rotation? (fh)
  ; (needs-rotation? properties) -> t | nil
  )

(defun rotate-logfile (fh)
  ; (rotate-logfile properties) -> properties
  fh)

(defun initialize-logfile-handler (cfg)
  ; (initialize-logfile-handler properties) -> properties
  ; Check
  ; Fully qualify name
  ; Ready new entries
  ; Open filestream
  ; Check for rotation
  cfg)

(defun create-log-file-handlers (cfg)
  ; Iterate through handlers looking for type :file
  (defq  fmap (hmap))
  ; For each, extend with file information and
  (each (lambda (ent)
          (print (getp (second ent) :type))
          ; rotate if specific and required TBD!
          )
        (entries (getp-in cfg :logging :handlers)))
  fmap)

(defun-bind process-log-cfg ()
  ; (process-log-cfg) -> tuple
  ; Sets up logging configuration from YAML or fall back to bare bones
  ; Sets up logging file handlers as needed/specified
  (defq
    cntrl_log (file-stream "./logs/logservice.log" file_open_append)
    cfg_age   (age +cfg_file+)
    cfg       nil)
  (setq cfg
        (if (> cfg_age 0)
          (first (yaml-read "./apps/logger/logsrvc.yaml"))
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
                  :handler :console_handler))
             :contexts (properties
                :service (properties
                  :handler :service_handler)
                :system (properties
                  :handler :system_handler))))))
  ; Build the system filesystem logger streams
  (defq fsmaps (create-log-file-handlers cfg))

  (list
    cntrl_log
    (> cfg_age 0)
    cfg
    fsmaps))

(defun-bind log-set-cfg (ucfg scfg)
  ; (log-set-cfg user-configuration service-configuration) -> nil
  ; Reconciles client configuration with service-configuration
  ; First set hash for user configuration
  (setsp! ucfg
    :token (hash ucfg)
    :levels (getp-in scfg :logging :levels))
  ; Resolve log handler and level
  (case (defq rl (getp (getp-in scfg :logging :loggers) (getp ucfg :logger)))
    ((nil)
     (setsp! ucfg
        :logger   :console
        :log_lvl  (getp-in scfg
                          :logging
                          :handlers
                          (getp-in scfg :logging :loggers :console :handler)
                          :level)))
    (t
      (setp! ucfg
        :log_lvl (getp-in scfg
                         :logging
                         :handlers
                         (getp rl :handler)
                         :level) t))))
