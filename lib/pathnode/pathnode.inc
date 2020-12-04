;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pathnode - File System folder paths for ChyrsaLisp
; Implements a tree hierarchy using the named-xnode
; collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "lib/clenv/clenv.inc")
(import "lib/date/date.inc")

(defq
  _pathseparator  "/"
  _pathnoderoot   nil
  _current_dir    nil)  ; default path separator

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

; Easy/default filters
(defq
    _pn-name-only    first
    _pn-all-filter   (#(t))
    _pn-file-filter  (#(eql (second %0) "8"))
    _pn-dir-filter   (#(eql (second %0) "4"))
    _pn_short-filter (lambda ((_fn _ft))
                            (when (and
                              (not (eql _fn "."))
                              (not (eql _fn ".."))
                              (not (eql (first _fn) "."))) t)))

(defclass path-node (name &optional parent)
  ; file-system path tree
  (def (defq this (named-xnode name parent))
    :path_node t
    :stat_res  nil)

  (when (not (get :stat_res this))
    (set this :stat_res (pii-fstat (. this :full_name))))

  (defmethod :date (this)
    ; (. path-node :date) -> date
    ; Returns node mod date from fstat
    (date (first (get :stat_res this))))

  (defmethod :size (this)
    ; (. path-node :size) -> number
    ; Returns node size from fstat
    (second (get :stat_res this)))

  (defmethod :mode (this)
    ; (. path-node :mode) -> number
    ; Returns node mode from fstat
    (last (get :stat_res this)))

  (defmethod :is_root? (this)
    (not (. this :get_parent)))

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
      name-fn _pn-name-only
      filter-fn _pn-all-filter)
    (map name-fn (filter filter-fn (. this :members))))

  (defmethod :dir-members (this &optional name-fn filter-fn)
    ; (. path-node :dir-members [name-fn filter-fn])
    ; HOF
    ; Returns directory type members of path-node [filter-fn]
    ; then maps results using name function [name-fn]
    (setd
      name-fn _pn-name-only
      filter-fn _pn-dir-filter)
    (. this :all-members name-fn filter-fn))

  (defmethod :file-members (this &optional name-fn filter-fn)
    ; (. path-node :file-members [name-fn filter-fn])
    ; HOF
    ; Returns file type members of path-node [filter-fn]
    ; then maps results using name function [name-fn]
    (setd
      name-fn _pn-name-only
      filter-fn _pn-file-filter)
    (. this :all-members name-fn filter-fn))

  (defmethod :fqname (this filename)
    ; (. path-node filename) -> string
    ; Returns fully qualifed filename prefixed by pathname
    (defq pn (. this :full_name))
    (str pn (if (eql (last pn) _pathseparator) "" _pathseparator) filename))

  (defmethod :expand-downto (this strpathlist)
    ; (. path-node :expand-downto path-node pathlist) -> path-node
    ; Builds out children to path provided in strpathlist
    ; throws exception if any path element is invalid
    (defun index-of (etf elist)
      (reduced-reduce (lambda (acc el)
        (if (eql el etf) (reduced _) nil)) elist t))

    (defun prior-segment (this)
      (defq p1 (. this :get_parent))
      (cond
        ; Root where this = first sep
        ((nil? p1)
          this)
        ; Parent is root
        ((nil? (. p1 :get_parent))
          p1)
        (t
          (. p1 :get_parent))))

    (reduce (lambda (acc el)
      (defq cn (. acc :child_for_name el))
      (print (. acc :unique_id) " Has child " cn " for " el)
      (cond
        ; Relative move
        ((eql el "..")
          (defq ps (prior-segment acc))
          (print "From " (. acc :unique_id) " to " (. ps :unique_id))
          ps)
        ; Separator
        ((eql el _pathseparator)
          (print "Separator returns same")
          acc)
        ; Child already exists
        (cn
          (print "Child " (. cn :unique_id))
          cn)
        ; Separator handling
        (t
          (print "Looking for " el " in " (. acc :dir-members))
          (if (index-of el (. acc :dir-members))
            (path-node el
              (if (. acc :is_root?)
                acc
                (path-node _pathseparator acc)))
            (throw
              (str el " is invalid child of path " (. acc :full_name))
              (. acc :dir-members))))
        )) strpathlist this))
  )

(defun path-node? (this)
  ; (path-node? obj) -> t | nil
  (and (env? this) (get :path_node this)))

; Initializing routine

(when (not _pathnoderoot)
  (setq
    _pathseparator (gets-enval "PATH_SEP")
    _pathnoderoot  (path-node _pathseparator)
    _current_dir   _pathnoderoot))


; Path string utilities

(defun _strpath-tolist (pathname &optional sep)
  ; (_strpath-tolist pathname) -> list
  ; Parses a pathname string into a list of path elements
  ; example:
  ; (_strpath-tolist "/Users/bob/here") -> ("/" "Users") "/" "bob" "/" "here")
  (setd sep _pathseparator)
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


(defun change-dir (pathname)
  ; (change-dir pathname ) -> path-node
  ; Returns the path-node for pathname
  ; Will expand tree by building valid path-nodes in pathname
  (defq
    path-el (_strpath-tolist pathname)
    frstel  (first path-el)
    plen    (length path-el))
  (print "First element " frstel " from " pathname " list " path-el)
  (setq _current_dir
    (cond
      ; Fully qualified from root
      ((and (eql frstel _pathseparator) (> plen 1))
        (. _pathnoderoot :expand-downto (slice 1 -1 path-el)))
      ; Root
      ((and (eql frstel _pathseparator) (= plen 1))
        _pathnoderoot)
      ; Relative parent path of current directory
      ((and (eql frstel "..") (> plen 1))
        (. _current_dir :expand-downto path-el))
      ((and (eql frstel "..") (= plen 1))
        (. _current_dir :get_parent))
      ; Relative current path
      ((and (empty? path-el) (eql (first pathname) "."))
        _current_dir)
      ((and (= plen 1) (eql (first frstel) ".") (eql (second frstel) _pathseparator))
        _current_dir)
      ((and
        (> plen 2)
        (eql frstel ".")
        (eql (second path-el) _pathseparator))
        (. _current_dir :expand-downto (slice 1 -1 path-el)))
      (t (throw "Unhandled change-dir " pathname))))
  _current_dir)

(defun build-path (pathname)
  ; (build-path pathname) -> path-node
  ; Parses pathname and returns a sparse path-node tree
  )
