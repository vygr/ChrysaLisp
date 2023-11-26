;;;;;;;;;;;;;;;;;;;
; pattern searching
;;;;;;;;;;;;;;;;;;;

(import "./charclass.inc")
(import "./substr.inc")
(import "./regexp.inc")

;module
(env-push)

(defq +substr (Substr 11) +regexp (Regexp 11))

(defun match? (_t _p)
	; (match? text regexp) -> :t | :nil
	(. +regexp :match? _t _p))

(defun substr (_t _p)
	; (substr text substr) -> (slices () ())
	(. +substr :search _t _p))

(defun matches (_t _p)
	; (matches text regexp) -> (slices subslices submatches)
	(. +regexp :search _t _p))

(defun query (_p _w _r)
	; (query pattern whole_words regexp) -> (engine pattern meta)
	(when (and (not (eql _p "")) _w (not _r))
		(setq _p (apply (const cat) (reduce
			(# (if (find %1 "^$!.*+?\|[]()")
				(push %0 "\")) (push %0 %1)) _p (list)))))
	(defq _e (if (or _w _r) +regexp +substr)
		_p (if _w (cat "!" _p "!") _p))
	(list _e _p (. _e :compile _p)))

;module
(export-symbols '(match? substr matches query))
(env-pop)
