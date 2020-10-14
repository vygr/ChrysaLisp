;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logutils - ChrysaLisp Logging Service utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defq
  +logs_path+ "./logs/"
  +log_suffix+ ".log"
  +cfg_file+   "./apps/logger/logsrvc.yaml")

(defun-bind process_log_cfg ()
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