;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/theme/theme.inc - the theme of a desktop
;
; A theme is which of the symbol fonts the toolbars are drawn with, there
; is more than one, the symbols command makes them. Which one a user has
; is kept in usr/<user>/theme, and is what an app starts with. It can
; be changed while apps are running: the GUI is told, (gui-theme-rpc), it
; tells every window, and a window swaps its fonts, lays itself out and
; draws again, (. window :event). The Themes app is how it is chosen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a theme, its name, the font of its symbols, and the font of the smallest
;of them, which want more weight than the rest
(defq *themes* (list
	(list "Regular" "fonts/Symbols.ctf" "fonts/Symbols-Bold.ctf")
	(list "Light" "fonts/Symbols-Light.ctf" "fonts/Symbols.ctf")
	(list "Bold" "fonts/Symbols-Bold.ctf" "fonts/Symbols-Bold.ctf")
	(list "Sharp" "fonts/Symbols-Sharp.ctf" "fonts/Symbols-Sharp.ctf")))

(defun theme-files (name)
	; (theme-files name) -> (symbols_file tiny_file)
	;the fonts of a theme, those of the first if there is none of that name
	(rest (ifn (some (# (if (eql (first %0) name) %0)) *themes*) (first *themes*))))

(defun theme-current (home)
	; (theme-current home) -> name
	;the theme a user has, home is usr/<user>/. The first if none was
	;ever chosen, or if what the file says is not a theme. The file is the
	;name and no more, it is read by every app as it starts, and there is
	;nothing in it to go wrong
	(defq name (trim (ifn (load (cat home "theme")) "") (const (char-class " \t\r\n"))))
	(if (some (# (eql (first %0) name)) *themes*) name (first (first *themes*))))

(defun theme-save (home name)
	; (theme-save home name)
	;the theme a user has from now on
	(save (cat name (ascii-char 10)) (cat home "theme")))
