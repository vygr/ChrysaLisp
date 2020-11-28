;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fauxfs - Faux File System for ChyrsaLisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro isfile? (mode)
  `(/= (logand mode 0x8000) 0))

(defmacro isdir? (mode)
  `(/= (logand mode 0x4000) 0))
