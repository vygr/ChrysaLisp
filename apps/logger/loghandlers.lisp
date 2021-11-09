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
  levels          nil
  formatting      (xmap)
  loggers         (xmap))

;
(defun _populate-class (_clzi fmap)
  ; (_populate-class instance map) -> any
  ; Populates a classes properties based
  ; on key/values from fmap
  (each (lambda ((_k _v))
          (def _clzi _k _v)) (entries fmap)))

(defun _level-index (lvlkw)
  (gets-in levels lvlkw))

(defun _level-name (lvlkw)
  (elem-get (_level-index lvlkw) (gets-in levels :logstrs)))

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
             (push msgs +nl)
             0
             (list (encode-date) " [" (_level-name lvl) "] "))))
  )

; Populate formatters map
(defun _add-formatter (name fmap)
  (defq fmtclass
    (cond
      ((eql name :standard) (stdformat name nil))
      (t (formatter name nil))))
  (_populate-class fmtclass fmap)
  (sets! formatting name fmtclass))

; low level IO log handlers
(defclass handler (name hnd_map) nil
  (def this
       :name        name
       :fmi         (gets formatting (gets hnd_map :formatter))
       :io_instance nil
       :written     0)

  (defabstractmethod :write (this lvl msgs))

  (defmethod :writer (this lvl msg)
    (cond
      ; If the inbound equals or is greater than handler threshold
      ((>= (_level-index lvl) (_level-index (get :level this)))
        (defq fmsg (. (get :fmi this) :formatmsg lvl msg))
        (stream-flush (write (get :io_instance this) fmsg))
        (length fmsg))
      (t 0)))

  ; Constructor for fields
  (_populate-class this hnd_map)
  )

(defclass console-handler (name hnd_map) (handler name hnd_map)

  (defmethod :write (this lvl msgs)
    ; (. console-handler :write [msgs])
    (set this :written (. this :writer lvl msgs)))

  )

(defclass file-handler (name hnd_map) (handler name hnd_map)

  (defmethod :write (this lvl msgs)
    ; (. file-handler :write level [msgs])
    (set this :written
         (. this :writer lvl msgs))
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
    (while (eql (elem-get 0 nm) +dblq)
      (setq nm (slice 1 -2 nm)))
    (set this :file_name nm)
    ; (when (eql (elem-get 0 nm) +dblq)
    ;   (set this :file_name (slice 1 -2 nm)))
    (bind '(fstream sz)
      (open-latest-log
        (get :name this)
        (get :file_name this)
        (if (get :rotate this) (get :backups this) +max_int)
        (if (get :rotate this) (get :maxbytes this) +max_int)))
      (set this
       :io_instance fstream
       :written     sz))
  )

; Primary logger

(defclass logger (name hnd_name hnd_map) nil
  (def this
       :name    name
       :handler nil)

  (defmethod :write (this src lvl &rest msgs)
    (. (get :handler this) :write lvl
       (insert msgs 0 (list (str (get :name this) ":" src " ")))))

  ; Constructor
  (set this
       :handler
       (cond
         ((eql (defq htype (gets hnd_map :type)) :stdout)
          (console-handler hnd_name hnd_map))
         ((eql htype :file)
          (file-handler hnd_name hnd_map))
         (t
           (throw "Unknown handler type" htype))))
  )

(defclass faux-logger () nil
  (defmethod :write (this src lvl &rest msgs)
    nil))


(defun load-loggers (&optional debuglogger)
  ; (load-loggers [debuglogger]) -> logger | nil

  (defq dbl nil)
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

  ; Load levels and normalize strings
  (setq levels (gets-in yamlmap :logging :levels))
  (sets! levels :logstrs
         (reduce (lambda (acc el)
                   (if (eql (first el) +dblq)
                       (push acc (slice 1 -2 el))
                       (push acc el)))
                 (gets levels :logstrs)
                 (list)))

  ; Load formatters
  (each (lambda ((_k _v)) (_add-formatter _k _v))
    (entries (gets-in yamlmap :logging :formatters)))

  ; Load debug logger if requested
  (setq dbl
    (if debuglogger
        (logger
          :debug
          :debug_handler
          (emap-kv
            :type :file
            :level :debug
            :formatter :standard
            :file_name "DEBUG"
            :rotate    nil
            :maxbytes  +max_int
            :backups  +max_int))
        (faux-logger)))

  ; Load configured loggers and handlers
  (each (lambda ((_k _v))
      (defq hnd_name (gets _v :handler))
      ; Create logger with handler
      (sets! loggers _k
             (logger _k
                     hnd_name
                     (gets-in yamlmap :logging :handlers hnd_name))))
    (entries (gets-in yamlmap :logging :loggers)))

  ; Return the debug logger or nil
  dbl)

(defun logger-for (lkeyword)
  ; (logger-for keyword) -> logger | nil
  ; Returns a logger with matching keyword name
  (gets loggers lkeyword))

(defun handler-for (lkeyword)
  ; (handler-for) -> handler | nil
  ; Returns a keyword matching logger's handler
  (when (defq logr (logger-for lkeyword))
    (get :handler logr)))

(defun config-for (name)
  ; (config-for name) -> keyword | nil
  ; Returns the handler keyword given a logger name that matches
  ; our configuration
  (when (defq lgr (gets-in yamlmap :logging :loggers (sym (cat : name))))
    (dbg-write (str "Handler for " name) (gets lgr :handler))
    (gets lgr :handler)))

(defun persist-loggers ()
  ; (persiste-loggers) -> any
  ; Writes the in memory configuration to
  ; file
  (yaml-write +ACTIVE-CNTRL+ yamlmap))

(defun new-logger (name logkey logcfg hndkey hndcfg)
  ; (new-logger name logger-key logger-cfg handler-key handler-cfg) -> t | nil
  ; Registers a new logger configuration and persists
 (sets! (gets-in yamlmap :logging :loggers) logkey logcfg)
 (sets! (gets-in yamlmap :logging :handlers) hndkey hndcfg)
 (persist-loggers)
 ; Instantiate new logger and handler if not already
 ; loaded
 (when (nil? (gets loggers logkey))
   (sets! loggers logkey (logger logkey hndkey hndcfg)))
 logkey)

