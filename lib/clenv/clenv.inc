;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; clenv - ChrysaLisp env configuration
; Loads and maintains persistence to .hostenv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Accessable configuration

(defq
  +source-host-env+ ".hostenv"
  +envfilekey+      "ENVCFG"
  +envcfg+          (xmap))

(defun __load-envcfg (filename)
  ; (__load-envcfg filename) -> nil
  ; Loads a map with key/value created from
  ; key=value
  ; etc.
  (defq fs (file-stream filename))
  (while (defq ln (read-line fs))
    (bind '(_k _v) (split ln "="))
    (sets! +envcfg+ _k (if (eql _v "nil") nil _v)))
  nil)

(defun gets-enval (key)
  ; (get-enval key) -> val | nil
  ; Gets value for key from environment configuration
  ; else returns nil
  (gets +envcfg+ key))

(defun __save-envcfg ()
  ; (__save-envcfg map filename) -> map
  ; Dumps map to configuration file lines
  ; key=value
  ; etc.
  (defq
    fs (file-stream (gets-enval +envfilekey+) file_open_write))
  (each (lambda ((_k _v))
    (write-line fs (str _k "=" _v))) (entries +envcfg+))
  (stream-flush fs)
  nil)

(defun exports-keyvals! (&rest kvs)
  ; (exports-keyvals k v ....) -> nil
  ; Adds or modifies values for keys and persists configuration
  (each (lambda ((_k _v)) (sets! +envcfg+ _k _v)) (partition 2 kvs))
  (__save-envcfg)
  nil)

(defun sets-envkvs! (&rest kvs)
  ; (sets-envkvs! k v ....) -> nil
  ; Adds or modifies values for keys
  (each (lambda ((_k _v)) (sets! +envcfg+ _k _v)) (partition 2 kvs))
  nil)

; Autoload the configuration
(when (empty? (entries +envcfg+))
  (if (= (age +source-host-env+) 0)
    (throw "Host environment file not found " +source-host-env+)
    (__load-envcfg +source-host-env+))
  (when (nil? (gets-enval +envfilekey+))
    (defq fsep (if (eql (gets-enval "OS") "Windows") (ascii-char 0x5c) "/"))
    (exports-keyvals!
      "PROMPT"      ">"
      "LASTC"       nil
      +envfilekey+  (str (gets-enval "PWD") fsep +source-host-env+)
      "PATH"        "cmd;apps"
      "PATH_SEP"    fsep)))

