;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a batch of samples picked out of a data set kept as bytes, a byte to
; an input, 0 to 255 as 0 to 1, and a byte to a label, by the index of
; each sample of the batch. An index for each input of the batch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the data set, its bytes 4 to an int, the lowest first
(defbuffer data :int :read)
;which sample each of the batch is
(defbuffer idx :int :read)
(defbuffer x :float :write)
(defbuffer y :int :write)
;how many inputs a sample has, and where the inputs and labels start in
;the data, in bytes
(definput ins :int 784)
(definput inputs :int 0)
(definput labels :int 0)

(defun main :void ((i :int))
	(defq s (/ i ins) k (- i (* s ins)) n (elem-get idx s) at (+ inputs (* n ins) k))
	(elem-set x i (/ (float (logand (>> (elem-get data (>> at 2)) (* (logand at 3) 8)) 255)) 255.0))
	(when (= k 0)
		(defq lat (+ labels n))
		(elem-set y s (logand (>> (elem-get data (>> lat 2)) (* (logand lat 3) 8)) 255))))
