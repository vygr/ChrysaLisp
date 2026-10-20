;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; huff, the Huffman books of sound and pictures read by native code,
; lib/media/lisp.vp media_huff, as AAC and MP3 have them
;
; (huff-table codes) makes the table of a book: codes is each code's
; length and value, in the order of what they stand for, as the tables
; of a standard give them. (media-huff data at table) reads one, native
; code taken by (ffi "lib/media/media_huff") where it is used.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun huff-table (codes)
	; (huff-table codes) -> nums
	;levels of 256, each by the next 8 bits of a code: a code that ends in
	;a level fills each entry its bits begin, what it stands for by 256 and
	;how many of the 8 it is; a longer one's entry is the next level, at
	;-entry. A list of the levels yet to make, each the entry of the one
	;before to link to it, its codes and the bits of them taken already
	(defq table (list) todo (list (list -1 (map (lambda ((n v)) (list (!) n v)) (partition codes 2)) 0)))
	(while (defq job (pop todo))
		(bind '(link level_codes used) job)
		(defq base (length table) subs (Fmap 31))
		(times 256 (push table 0))
		(if (>= link 0) (elem-set table link (neg base)))
		(each (lambda ((sym n v))
				(defq left (- n used))
				(cond
					((<= left 8)
						(defq from (<< (logand v (dec (<< 1 left))) (- 8 left)))
						(each (# (elem-set table (+ base from %0) (+ (<< sym 8) left))) (range 0 (<< 1 (- 8 left)))))
					(:t (defq at (logand (>> v (- left 8)) 255))
						(. subs :insert at (push (ifn (. subs :find at) (list)) (list sym n v))))))
			level_codes)
		(. subs :each (lambda (at list) (push todo (list (+ base at) list (+ used 8))))))
	(reduce (# (push %0 %1)) table (cap (length table) (nums))))

;module
(export-symbols '(huff-table))
(env-pop)
