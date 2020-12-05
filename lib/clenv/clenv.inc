;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; clenv - ChrysaLisp env configuration
; Loads and maintains persistence to .hostenv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Accessable configuration

(defq
  +source-host-env+ ".hostenv"
  _envfilekey       nil
  +envcfg+          (xmap))

(defun __load-envcfg (filename)
  ; (__load-envcfg filename) -> nil
  ; Loads a map with key/value created from
  ; key=value
  ; etc.
  (defq fs (file-stream filename file_open_read))
  (while (defq ln (read-line fs))
    (bind '(_k _v) (split ln "="))
    (sets! +envcfg+ _k (if (eql _v "nil") nil _v)))
  nil)

(defun gets-enval (key)
  ; (gets-enval key) -> val | nil
  ; Gets value for key from environment configuration
  ; else returns nil
  (gets +envcfg+ key))

(defun __save-envcfg ()
  ; (__save-envcfg map filename) -> map
  ; Dumps map to configuration file lines
  ; key=value
  ; etc.
  (defq
    fs (file-stream _envfilekey file_open_write))
  (each (lambda ((_k _v))
    (write-line fs (str _k "=" _v))) (entries +envcfg+))
  (stream-flush fs))

(defun exports-keyvals! (&rest kvs)
  ; (exports-keyvals k v ....) -> nil
  ; Adds or modifies values for keys and persists configuration
  (each (lambda ((_k _v)) (sets! +envcfg+ _k _v)) (partition 2 kvs))
  (__save-envcfg))

(defun sets-envkvs! (&rest kvs)
  ; (sets-envkvs! k v ....) -> nil
  ; Adds or modifies values for keys
  (each (lambda ((_k _v)) (sets! +envcfg+ _k _v)) (partition 2 kvs)))

(defun __one_time ()
  ; We need the .hostenv from root at least
  (if (= (age +source-host-env+) 0)
    (throw "Host environment file not found " +source-host-env+)
    (__load-envcfg +source-host-env+))
  ; Separator setup and permanent file location
  (defq
    fsep  (if (eql (gets-enval "OS") "Windows") (ascii-char 0x5c) "/"))
  (setq _envfilekey (str (gets-enval "HOME") fsep +source-host-env+))
  ; When there is no hostenv yet
  (if (= (age _envfilekey) 0)
    (exports-keyvals!
      "PROMPT"      ">"
      "LASTC"       nil
      "PATH"        "cmd;apps"
      "PATH_SEP"    fsep)
    ; Otherwise load existing
    (__load-envcfg _envfilekey)))

; Autoload the configuration
(when (nil? _envfilekey)
  (__one_time)
  (undef (env) '__load-envcfg '__one_time))




