;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pathnode - File System folder paths for ChyrsaLisp
; Implements a tree hierarchy using the named-xnode
; collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "lib/clenv/clenv.inc")
(import "lib/date/date.inc")

(defq
  _pathseparator  "/"   ; default path separator
  _pathnoderoot   nil
  _current_dir    nil)

; File mode masks

(defmacro isfile? (stat)
  `(/= (logand (last ,stat) 0x8000) 0))

(defmacro isdir? (stat)
  `(/= (logand (last ,stat) 0x4000) 0))

(defmacro user-mask (stat)
  `(logand (last ,stat) 0xe00))

(defmacro user-id-mask (stat)
  `(logand (last ,stat) 0x800))

(defmacro group-id-mask (stat)
  `(logand (last ,stat) 0x400))

(defmacro sticky-bit-mask (stat)
  `(logand (last ,stat) 0x200))

; These generalize to first 3 bits
(defmacro owner-mask (stat)
  `(>> (logand (last ,stat) 0xfff) 6))
(defmacro group-mask (stat)
  `(>> (logand (last ,stat) 0xfff) 3))
(defmacro other-mask (stat)
  `(logand (last ,stat) 0x007))

;
(defmacro noperm? (stat)
  `(= (last ,stat) 0))
(defmacro read? (mask)
  `(/= (logand ,mask 4) 0))
(defmacro write? (mask)
  `(/= (logand ,mask 2) 0))
(defmacro exec? (mask)
  `(/= (logand ,mask 1) 0))

; String utilities

; " nnn sz "
(defun fsize-str (size)
  ; (fsize-str size) -> string
  ; Converts a file size to simple 3 dig with suffix
  (defq
    sizes-in (list "   " " KB" " MB" " GB")
    fsize 1000
    bsize 0)
  (while (> size fsize)
    (setq
      fsize 1024
      size (/ size fsize)
      bsize (inc bsize)))
  (str " " (pad size 3 " ") (elem-get bsize sizes-in)))

(defun mode-str (stat)
  ; (mode-str stat) -> string
  ; Converts the file/directory stat to permissions string
  (defq
    fstr (list (if (isdir? stat) "d" "-"))
    plst (list (owner-mask stat) (group-mask stat) (other-mask stat)))
  (each (lambda (_msk)
    (push fstr
      (if (read? _msk) "r" "-")
      (if (write? _msk) "w" "-")
      (if (exec? _msk) "x" "-"))
    ) plst)
  (join fstr ""))

; Easy/default filters
(defq
    _pn-name-only    first
    _pn-all-filter   (#(eql %0 %0))
    _pn-dir-filter   (#(eql (second %0) "4"))
    _pn_short-filter (lambda ((_fn _ft))
                            (when (and
                              (not (eql _fn "."))
                              (not (eql _fn ".."))
                              (not (eql (first _fn) "."))) t))
    _pn_nofind-handler (#(reduced (list %0 %1 %2))))

; path-node class

(defclass path-node (name &optional parent) (named-xnode name parent)
  ; file-system path tree
  (def this
    :path_node t
    :stat_res  nil)

  (defmethod :full_path (this)
    ; (. path-node :full_path) -> string
    ; Returns the full path string
    (. this :full_name))

  ; Set stats for node
  (when (not (get :stat_res this))
    (set this :stat_res (pii-fstat (. this :full_path))))

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
    ; (. path-node :is_root?) -> t | nil
    ; Tests whether this node is the root
    (not (. this :get_parent)))

  (defmethod :members (this)
    ; (. path-node :members) -> list
    ; Returns list of pairs from pii-dirlist
    (partition 2 (split (pii-dirlist (. this :full_path)) ",")))

  (defmethod :all_members (this &optional name-fn filter-fn)
    ; (. path-node :all_members [name-fn filter-fn])
    ; HOF
    ; Returns filtered members of path-node [filter-fn]
    ; then maps results using name function [name-fn]
    ; Defaults to all members with all names (including hidden dirs and files)
    (setd
      name-fn _pn-name-only
      filter-fn _pn-all-filter)
    (map name-fn (filter filter-fn (. this :members))))

  (defmethod :dir_members (this &optional name-fn filter-fn)
    ; (. path-node :dir_members [name-fn filter-fn])
    ; HOF
    ; Returns directory type members of path-node [filter-fn]
    ; then maps results using name function [name-fn]
    (setd
      name-fn _pn-name-only
      filter-fn _pn-dir-filter)
    (. this :all_members name-fn filter-fn))


  (defmethod :fqname (this filename)
    ; (. path-node filename) -> string
    ; Returns fully qualifed filename prefixed by pathname
    (defq pn (. this :full_path))
    (str pn (if (eql (last pn) _pathseparator) "" _pathseparator) filename))

  (defmethod :child_segment (this node_name)
    ; (. path-node :child_segment node_name)
    (cond
      ((. this :is_root?)
        (. this :child_node node_name))
      ((defq snode (. this :child_node _pathseparator))
        (. snode :child_node node_name))
      (t nil)))

  (defmethod :prior_segment (this)
    ; (. path-node :prior_segment) -> path-node
    (defq p1 (. this :get_parent))
    (cond
      ; Root where this = first sep
      ((nil? p1) this)
      ; Parent is root
      ((nil? (. p1 :get_parent)) p1)
      (t (. p1 :get_parent))))

  (defmethod :new_child_segment (this node_name)
    ; (. path-node :new_segment node_name)
    ; Adds a new child segment with separator and node if
    ; doesn't exist
    (cond
      ; No need to add separator to root as it is already
      ((and (. this :is_root?) (eql node_name _pathseparator))
        this)
      ; No need to add separator of root, just add new
      ((. this :is_root?)
        (. this :add_node (path-node node_name)))
      ; Add a separator as child of this and then new to that
      (t
        (defq msep (opt
          (. this :has_child? _pathseparator)
          (. this :add_node (path-node _pathseparator))))
        (opt
          (. msep :has_child? node_name)
          (. msep :add_node (path-node node_name))))))


  (defmethod :expandto (this strpathlist &optional handler_fn)
    ; (. path-node :expandto path-node pathlist) -> path-node
    ; Builds out children to path provided in strpathlist
    ; throws exception if any path element is invalid
    (reduced-reduce (lambda (acc el)
      (defq cn (. acc :child_segment el))
      (cond
        ; Relative moves
        ((or (eql el ".") (eql el _pathseparator))
          acc)
        ; Back up
        ((eql el "..")
          (. acc :prior_segment))
        ; Child already exists
        (cn cn)
        ; Fall through, validate child and add
        (t
          (if (find el (. acc :dir_members))
              (. acc :new_child_segment el)
              (if handler_fn
                (handler_fn el acc _)
                (throw
                  (str el " is invalid child of path " (. acc :full_path))
                  (. acc :dir_members))))))) strpathlist this))
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

(defun _path-tolist (pathname &optional sep)
  ; (_path-tolist pathname) -> list
  ; Parses a pathname string into a list of path elements
  ; example:
  ; (_path-tolist "/Users/bob/here") -> ("/" "Users") "/" "bob" "/" "here")
  ; (_path-tolist "/Users/bob/here" "\") -> ("\" "Users") "\" "bob" "\" "here")
  (setd sep _pathseparator)
  (defq
    seed (if (eql (first pathname) sep) (list sep) (list))
    prsd (split pathname sep)
    lng  (dec (length prsd)))
  (reduce
    (lambda (acc el)
      ; Expansion of '~'
      (when (eql el "~")
        (defq isp (split (gets-enval "HOME") sep))
        (each! 0 -2 (#(push acc sep %0 sep)) (list isp))
        (setq el (last isp)))
      (if (= _ lng)
        (push acc el)
        (push acc el sep)))
    prsd seed))

(defun node-for (pathname &optional handler_fn)
  ; (node-for pathname [exception_handler] ) -> path-node
  ; Returns the path-node for pathname
  ; Will expand tree by building valid path-nodes in pathname
  (defq
    path-el (_path-tolist pathname)
    frstel  (first path-el)
    plen    (length path-el))
  (cond
    ; Do nothing
    ((empty? path-el)
      _current_dir)
    ; Root
    ((and (eql frstel _pathseparator) (= plen 1))
      _pathnoderoot)
    ; Fully qualified from root
    ((and (eql frstel _pathseparator) (> plen 1))
      (. _pathnoderoot :expandto path-el handler_fn))
    ; Relative current path
    (t
      (. _current_dir :expandto path-el handler_fn))))

(defun make-dir (pathname &optional coe)
  ; (make-dir pathname [:exception | not-nil]) -> nil | exception
  ; Attempts to create, on the fs, pathname
  ; If coe not= :exception then all segments will be built
  ; If coe nil, then exception is default
  ; The default will require that the last element is the
  ; only new one to succeed

  (setd coe :exception)
  (cond
    ; Create all paths
    ((not (eql coe :exception))
      (defq fpath nil)
      (cond
        ; Releative up one
        ((starts-with (str ".." _pathseparator) pathname)
          (setq fpath
            (.
              (. _current_dir :prior_segment)
              :fqname (slice 3 -1 pathname))))
        ; Relative to current dir
        ((starts-with (str "." _pathseparator) pathname)
          (setq fpath (. _current_dir :fqname (slice 2 -1 pathname))))
        ; Root based
        ((starts-with _pathseparator pathname)
          (setq fpath pathname))
        ; Current dir based
        (t
          (setq fpath (. _current_dir :fqname pathname))))
      (file-stream (str fpath _pathseparator) +file_open_write))
      ; (when (nil? (defq oc (file-stream fpath +file_open_write)))
      ;   (throw (str "permission exception creating directory " fpath) oc))
    (t
      (defq
        pname pathname
        psplt (_path-tolist pname)
        fpath nil)
      (when (list? (defq mres (node-for pname _pn_nofind-handler)))
        ; (
        ;   segname - Offending path segment
        ;   pnode - path-node where last valid
        ;   plistndx - index of offending item in path-list
        ; )
        (bind '(segname pnode plistndx) mres)
        (when (> (length (slice plistndx -1 psplt)) 1)
          (throw (str "missing intermediate paths " pathname " from ")
            (. pnode :full_path)))
        (file-stream (str (. pnode :fqname segname) _pathseparator) +file_open_write)))))
        ; (when (nil? (defq oc (file-stream fpath +file_open_write)))
        ;   (throw (str "permission exception creating directory " fpath) oc))

(defun change-dir (pathname)
  ; (change-cdir pathname ) -> path-node
  ; Returns the path-node for pathname
  ; Will expand tree by building valid path-nodes in pathname
  (setq _current_dir (node-for pathname)))
