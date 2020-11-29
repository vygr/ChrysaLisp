;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fauxfs - Faux File System for ChyrsaLisp
; Implements a tree hierarchy using the named-xnode
; collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defmacro isfile? (mode)
  `(/= (logand ,mode 0x8000) 0))

(defmacro isdir? (mode)
  `(/= (logand ,mode 0x4000) 0))

(defun permissions-to-str (mode)
  ; (permission-to-str)
  "-----------")

(defun dir-listing (path)
  ; (dir-listing path) -> list
  ; Creates pathname type tuples
  (partition 2 (split (pii-dirlist path) ",")))

(defun file-lists (targs)
  ; (file-lists pathlist) -> list
  ; For each path in targs builds a dir listing tuple
  ; (path ((child1 type1) (childn typen)))
  (reduce (#(push %0 (list %1 (dir-listing %1)))) targs (list)))

(defun files-only (dirlist)
  ; (files-only dirlisting) -> list
  ; Filters in only file types
  (filter
      (lambda ((_fn _ft)) (and (eql _ft "8") (not (eql (first _fn) "."))))
      dirlist))

(defun files-only-short (flist)
  (reduce (#(push %0 (first %1))) (files-only flist) (list)))
