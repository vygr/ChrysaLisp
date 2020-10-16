;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logutils - ChrysaLisp Logging Service utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defq
  +logs_path+ "./logs/"
  +log_suffix+ ".log"
  +cfg_file+   "./apps/logger/logsrvc.yaml")

(defun-bind process-log-cfg ()
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
                                     :critical  4)
                           :formatters (properties
                                         :standard (properties
                                                     :format "tnlm"))
                           :handlers (properties
                                       :console_handler (properties
                                                          :type :stdout
                                                          :level :info
                                                          :formatter :standard))
                           :loggers (properties
                                      :console (properties
                                                 handler: :console_handler))))))
  (list cntrl_log (> cfg_age 0) cfg))

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
        :log_lvl  (get-in scfg
                          :logging
                          :handlers
                          (get-in scfg :logging :loggers :console :handler)
                          :level))
    (t
      (setp! ucfg
        :log_lvl (get-in scfg
                         :logging
                         :handlers
                         (getp rl :handler)
                         :level)
        t)))))
