;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; loghandlers - Service loggers and handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/date/date.inc")
(import "lib/xchange/yaml-data.inc")
(import "apps/logger/logfiles.lisp")

(defq
  +SERVICE-CNTRL+ "./apps/logger/logsrvc.yaml"
  +ACTIVE-CNTRL+  "./logs/logdefs.yaml"
  yamlmap         nil
  formatting      (xmap)
  loggers         (xmap))

;
(defun populate-class (_clzi fmap)
  (each (lambda ((_k _v))
          (def _clzi _k _v)) (entries fmap)))

(defun level-index (lvlkw)
  (gets-in yamlmap :logging :levels lvlkw))

(defun level-name (lvlkw)
  (elem (level-index lvlkw) (gets-in yamlmap :logging :levels :logstrs)))

;;;;;;;;;;;;;;;;;;;;;
; Formatting
;;;;;;;;;;;;;;;;;;;;;

(defclass formatter (name fmt) nil
  (def this
       :name   name
       :format fmt)
  )

; standard formatter
(defclass stdformat (name fmt) (formatter name fmt)
  (defmethod :formatmsg (this lvl msgs)
    (apply str
           (insert
             (push msgs +nl+)
             0
             (list (encode-date) " [" (level-name lvl) "] "))))
  )

; Populate formatters map
(defun add-formatter (name fmap)
  (defq fmtclass
    (cond
      ((eql name :standard) (stdformat name nil))
      (t (formatter name nil))))
  (populate-class fmtclass fmap)
  (sets! formatting name fmtclass))

; low level IO log handlers
(defclass handler (name hnd_map) nil
  (def this
       :name        name
       :fmi         (gets formatting (gets hnd_map :formatter))
       :io_instance nil
       :written     0)

  (defabstractmethod :write (this lvl &rest msg))

  (defmethod :writer (this lvl msg)
    (cond
      ((<= (level-index lvl) (level-index (get :level this)))
        (defq fmsg (. (get :fmi this) :formatmsg lvl msg))
        (stream-flush (write (get :io_intance this) fmsg))
        (length fmsg))
      (t 0)))

  ; Constructor for fields
  (populate-class this hnd_map)
  )

(defclass console-handler (name hnd_map) (handler name hnd_map)

  (defmethod :write (this lvl &rest msg)
    ; (. console-handler :write [msgs])
    (set this :written (. this :writer lvl msg)))

  )

(defclass file-handler (name hnd_map) (handler name hnd_map)

  (defmethod :write (this lvl &rest msg)
    ; (. file-handler :write level [msgs])
    (set this :written (. this :writer lvl msg))
    (when (and
            (get :rotate this)
            (> (get :written this) (get :maxbytes this)))
      (bind '(fstream sz) (log-threshold (get :name this) (get :backups this)))
      (set this
           :io_instance fstream
           :written     sz)))

  ; Constructor

  (progn
    (defq nm (get :file_name this))
    (when (eql (elem 0 nm) +dblq+)
      (set this :file_name (slice 1 -2 nm)))
    (bind '(fstream sz)
      (open-latest-log
        (get :name this)
        (get :file_name this)
        (if (get :rotate this) (get :backups this) +max_int+)
        (if (get :rotate this) (get :maxbytes this) +max_int+)))
      (set this
       :io_instance fstream
       :written     sz))
  )

; logger
(defclass logger (name hnd_name hnd_map) nil
  (def this
       :name    name
       :handler nil)
  (set this :handler
       (cond
         ((eql (defq htype (gets hnd_map :type)) :stdout)
          (console-handler hnd_name hnd_map))
         ((eql htype :file)
          (file-handler hnd_name hnd_map))
         (t
           (throw "Unknown handler type" htype))))
  )

(defclass debug-logger (name) nil
  )


(defun debug-logger (&optional fake)
  )

(defun logger-configured? (lkeyword)
  (gets-in yamlmap :logging :loggers lkeyword))

(defun load-loggers ()
  ; Definition file. If the operational
  ; file does not exist, create
  ; from the static model. Otherwise just read
  ; operational
  (cond
    ((= (age +ACTIVE-CNTRL+) 0)
     (yaml-write
       +ACTIVE-CNTRL+
       (setq yamlmap (first (yaml-read +SERVICE-CNTRL+)))))
    (t
      (setq yamlmap (first (yaml-read +ACTIVE-CNTRL+)))))
  ; Load formatters
  (each (lambda ((_k _v)) (add-formatter _k _v))
    (entries (gets-in yamlmap :logging :formatters)))

  ; Load loggers and handlers
  (each (lambda ((_k _v))
      (defq hnd_name (gets _v :handler))
      ; Create logger with handler
      (sets! loggers _k
             (logger _k
                     hnd_name
                     (gets-in yamlmap :logging :handlers hnd_name))))
    (entries (gets-in yamlmap :logging :loggers)))
  )

(defun persist-loggers ()
  (yaml-write +ACTIVE-CNTRL+ yamlmap))

(defun new-logger (name handle-spec)
  )

