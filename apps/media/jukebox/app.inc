;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; what the window and its tasks share, ./player.lisp, ./measure.lisp,
; ./fetch.lisp and ./index.lisp: the rate a song is played at, the bars of
; its spectrum, and how long a task that hears nothing of the window lasts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +rate 44100 +bars 32 +orphan 5000000)
