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
	(or (Emap? _) (Xmap? _) (Fmap? _) (Lmap? _)))

(defun set? (_)
	; (set? object) -> :t | :nil
	; returns true if argument is a set type
	(or (Xset? _) (Fset? _)))

(defun gather (m &rest _)
	; (gather map key ...) -> (val ...)
	; gather a list of values
	(map (# (. m :find %0)) _))
