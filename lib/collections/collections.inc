;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; collections - ChyrsaLisp data collections library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/collections/emap.inc")
(import "lib/collections/xmap.inc")
(import "lib/collections/xset.inc")
(import "lib/collections/hmap.inc")

(defun-bind collection? (_)
    (or (emap? _)
        (xmap? _)
        (xset? _)
        (lst? _)))
