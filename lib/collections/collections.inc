;;;;;;;;;;;;;
; collections
;;;;;;;;;;;;;

(import "./emap.inc")
(import "./fmap.inc")
(import "./fset.inc")
(import "./xmap.inc")
(import "./xset.inc")
(import "./lmap.inc")

(defun map? (_)
	; (map? object) -> :t | :nil
	; returns true if argument is a map type
	(or (emap? _) (xmap? _) (fmap? _) (lmap? _)))

(defun set? (_)
	; (set? object) -> :t | :nil
	; returns true if argument is a set type
	(or (xset? _) (fset? _)))

(defun gather (m &rest _)
	; (gather map key ...) -> (val ...)
	; gather a list of values
	(map (# (. m :find %0)) _))
