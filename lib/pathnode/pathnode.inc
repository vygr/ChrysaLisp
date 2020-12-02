;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pathnode - File System folder paths for ChyrsaLisp
; Implements a tree hierarchy using the named-xnode
; collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

; File mode masks

(defmacro isfile? (mode)
  `(/= (logand ,mode 0x8000) 0))
(defmacro isdir? (mode)
  `(/= (logand ,mode 0x4000) 0))

(defmacro user-mask (mode)
  `(logand ,mode 0xe00))
(defmacro user-id-mask (mode)
  `(logand ,mode 0x800))
(defmacro group-id-mask (mode)
  `(logand ,mode 0x400))
(defmacro sticky-bit-mask (mode)
  `(logand ,mode 0x200))

; These generalize to first 3 bits
(defmacro owner-mask (mode)
  `(>> (logand ,mode 0x700) 8))
(defmacro group-mask (mode)
  `(>> (logand ,mode 0x070) 4))
(defmacro other-mask (mode)
  `(logand ,mode 0x007))

(defmacro noperm? (mode)
  `(= ,mode 0))
(defmacro read? (mode)
  `(/= (logand ,mode 4) 0))
(defmacro write? (mode)
  `(/= (logand ,mode 2) 0))
(defmacro exec? (mode)
  `(/= (logand ,mode 1) 0))

(defclass path-node (name &optional parent)
  ; file-system path tree
  (def (defq this (named-xnode name parent))
    :default-name-only first
    :default-all-filter (#(or %0 %0))
    :default-file-filter (#(eql (second %0) "8"))
    :default-dir-filter  (#(eql (second %0) "4"))
    :path_node t
    :stat_res  nil)

  (defmethod :members (this)
    ; (. path-node :members) -> list
    ; Returns list of pairs from pii-dirlist
    (partition 2 (split (pii-dirlist (. this :full_name)) ",")))

  (defmethod :all-members (this &optional name-fn filter-fn)
    ; (. path-node :all-members [name-fn filter-fn])
    ; HOF
    ; Returns filtered members of path-node [filter-fn]
    ; then maps results using name function [name-fn]
    ; Defaults to all members with all names (including hidden dirs and files)
    (setd
      name-fn (get :default-name-only this)
      filter-fn (get :default-all-filter this))
    (map name-fn (filter filter-fn (. this :members))))

  (defmethod :dir-members (this &optional name-fn filter-fn)
    ; (. path-node :dir-members [name-fn filter-fn])
    ; HOF
    ; Returns directory type members of path-node [filter-fn]
    ; then maps results using name function [name-fn]
    (setd
      name-fn (get :default-name-only this)
      filter-fn (get :default-dir-filter this))
    (. this :all-members name-fn filter-fn))

  (defmethod :file-members (this &optional name-fn filter-fn)
    ; (. path-node :file-members [name-fn filter-fn])
    ; HOF
    ; Returns file type members of path-node [filter-fn]
    ; then maps results using name function [name-fn]
    (setd
      name-fn (get :default-name-only this)
      filter-fn (get :default-file-filter this))
    (. this :all-members name-fn filter-fn))

  (defmethod :fqname (this filename)
    ; (. path-node filename) -> string
    ; Returns fully qualifed filename prefixed by pathname
    (defq pn (. this :full_name))
    (str pn (if (eql (last pn) "/") "" "/") filename))
  )

(defun path-node? (this)
  ; (path-node? obj) -> t | nil
  (and (env? this) (get :path_node this)))

; Path string utilities

(defq _dps  "/")  ; default path separator

(defun _strpath-tolist (pathname &optional sep)
  ; (_strpath-tolist pathname) -> list
  ; Parses a pathname string into a list of path elements
  ; example:
  ; (_strpath-tolist "/Users/bob/here") -> ("/" "Users") "/" "bob" "/" "here")
  (setd sep _dps)
  (reduce (lambda (acc el)
    (bind '(plen path_list wip) acc)
    (cond
      ; Last input character
      ((= _ plen)
        (when (nempty? wip)
          (push path_list (join (push wip el) "")))
        path_list)
      ; Path segment suffix
      ; Ignores redundant sep's (i.e. //)
      ((and (eql el sep) (empty? wip))
        (when (not (eql sep (last path_list)))
          (push path_list el))
        acc)
      ; Path segment prefix
      ((and (eql el sep) (nempty? wip))
        (push path_list (join wip "") el)
        (elem-set 2 acc (list))
        acc)
    (t
        (push wip el)
        acc)))
    pathname (list (dec (length pathname)) (list) (list))))

; FS Root, path functions and utilities
; (defq   _fs_root nil)  ; Set before other paths built

; (when (nil? _fs_root)
;   (setq _fs_root (path-node sep)))

(defun initialize-path (pathname os_separator)
  ; (initialize-path pathname os_separator) -> path-node
  ; Initializes usage of path-nodes by configuring
  ; os dependant path information then building a
  ; path hierarcy
  ; Returns the path-node for pathname
  (setq _dps os_separator)
  (defq
    pp (_strpath-tolist pathname)
    fp (first pp))
  (reduce (lambda (acc el)
      (defq nn (path-node el))
      (. acc :add_node nn))
    (slice 1 -1 pp) (path-node fp)))


(defun build-path (pathname)
  ; (build-path pathname) -> path-node
  ; Parses pathname and returns a sparse path-node tree
  )
