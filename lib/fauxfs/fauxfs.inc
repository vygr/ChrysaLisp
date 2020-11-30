;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fauxfs - Faux File System for ChyrsaLisp
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
    :path_node t)

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

; Utilities and misc functions

(defun path-node? (this)
  ; (path-node? obj) -> t | nil
  (and (env? this) (get :path_node this)))

(defun build-path (pathname)
  ; (build-path pathname) -> path-node
  ; Parses pathname and returns a sparse path-node tree
  (defq pp
    (reduce (lambda (acc el)
      (cond
        ((= _ (first acc))
          (when (nempty? (last acc))
            (push (second acc) (join (push (last acc) el) "")))
          (second acc))
        ((and (eql el "/") (empty? (last acc)))
          (push (second acc) el)
          acc)
        ((and (eql el "/") (nempty? (last acc)))
          (push (second acc) (join (last acc) "") el)
          (elem-set 2 acc (list))
          acc)
      (t
          (push (last acc) el)
          acc)))
    pathname (list (dec (length pathname)) (list) (list))))
  (reduce
      (#(path-node %1 %0))
    (slice 1 -1 pp) (path-node (elem 0 pp))))

