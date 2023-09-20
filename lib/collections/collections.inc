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

(defun scatter (m &rest _)
	; (scatter map key val ...) -> map
	; scatter a list of values
	(reduce (lambda (m (k v)) (. m :insert k v)) (partition 2 _) m))

;;;;;;;;;;;;;;;;;;;;
; map tree load/save
;;;;;;;;;;;;;;;;;;;;

(defun maptree-encode (atom)
	; (maptree-encode atom) -> atom
	(cond
		((sym? atom) atom)
		((num? atom) (str atom))
		((cat {"} (str atom) {"}))))

(defun maptree-save (stream root &optional key_filter)
	; (maptree-save stream maptree &optional key_filter) -> maptree
	(setd key_filter '())
	(defq out (list) stack (list root out)
		ws (const (pad "" 8 (ascii-char 9))) wc 1)
	(while (defq l (pop stack) m (pop stack))
		(. m :each (lambda (k v)
			(if (map? v) (push stack v (setq v (list))))
			(push l (list k v)))))
	(push stack out 0)
	(write-line stream "( ;map")
	(while (defq i (pop stack) l (pop stack))
		(defq p (pad "" wc ws))
		(cond
			((< i (length l))
				(bind '(k v) (elem-get i l))
				(push stack l (inc i))
				(when (notany (# (eql k %0)) key_filter)
					(setq k (maptree-encode k))
					(cond
						((list? v)
							;indent
							(setq wc (inc wc))
							(push stack v 0)
							(write-line stream (cat p k))
							(write-line stream (cat p "( ;map")))
						(:t (write-line stream (cat p k " " (maptree-encode v)))))))
			(:t ;outdent
				(write-line stream (cat (rest p) ")"))
				(setq wc (dec wc)))))
	root)

(defun maptree-load (stream)
	; (maptree-load stream) -> maptree
	(defq root (Fmap) stack (list (first (read stream (ascii-code " "))) root))
	(while (defq m (pop stack) form (pop stack))
		(each (lambda ((k v))
			(if (list? v) (push stack v (setq v (Fmap))))
			(. m :insert k v)) (partition 2 form)))
	root)
