;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; collections - ChyrsaLisp data collections library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/collections/emap.inc")
(import "lib/collections/xmap.inc")
(import "lib/collections/xset.inc")
(import "lib/collections/hmap.inc")

(defun-bind map? (_)
    ; (map? object) -> t | nil
    ; Returns true if argument is a map type
    (or (emap? _) (xmap? _) (hmap? _)))

(defun-bind collection? (_)
    ; (collection? obj) -> t | nil
    ; Returns true if argument is a collection type
    (or (map? _) (lst? _)))
