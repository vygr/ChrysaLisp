(import "././array/struct.inc")

(structure +list +array_size)

;a note of a list being printed, on the stack of its print, the next note
;and the list, see (. list :print_enter)
(structure +print_note 0
	(ptr next list))
