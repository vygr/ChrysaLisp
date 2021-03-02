;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; clenv - ChrysaLisp env configuration
; Loads and maintains persistence to .hostenv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Accessable configuration

(defq
  +source-host-env+ ".hostenv"
  _envfilekey       nil
  +envcfg+          (xmap))

(defun __load-envcfg (filename &optional cfgmap)
  ; (__load-envcfg filename) -> nil
  ; Loads a map with key/value created from
  ; key=value
  ; etc.
  (setd cfgmap +envcfg+)
  (defq fs (file-stream filename +file_open_read+))
  (while (defq ln (read-line fs))
    (bind '(_k _v) (split ln "="))
    (sets! cfgmap _k (if (eql _v "nil") nil _v)))
  cfgmap)

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
    fs (file-stream _envfilekey +file_open_write+))
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
  ; (__one_time) -> nil

  ; We need the .hostenv from root at least
  (defq
    bmap (xmap))

  ; Check for base configuration, except if not found
  ; otherwise load
  (if (= (age +source-host-env+) 0)
    (throw "Host environment file not found " +source-host-env+)
    (__load-envcfg +source-host-env+ bmap))

  ; Separator setup and permanent file location
  (defq
    fsep  (if (eql (gets-enval "OS") "Windows") (ascii-char 0x5c) "/"))
  (setq _envfilekey (str (gets bmap "HOME") fsep +source-host-env+))

  ; When there is no hostenv yet for user
  (if (= (age _envfilekey) 0)
    (progn
      (merge-into! +envcfg+ bmap)
      (exports-keyvals!
        "PROMPT"      ">"
        "LASTC"       nil
        "PATH"        "cmd;apps"
        "PATH_SEP"    fsep))
    (progn
      ; Load the existing user .hostenv
      ; And compare for differences
      (__load-envcfg _envfilekey)
      (if (not (eql (defq cv (gets bmap "HE_VER")) (gets +envcfg+ "HE_VER")))
        (sets! +envcfg+ "HE_VER" cv))
      (each (lambda ((_k _v))
        (when (nil? (gets +envcfg+ _k))
          (sets! +envcfg+ _k _v))) (entries bmap))
      (__save-envcfg))))

; Autoload the configuration
(when (nil? _envfilekey)
  (__one_time)
  (undef (env) '__load-envcfg '__one_time))




