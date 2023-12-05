;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-reset ()
	(route))

(defun action-prev ()
	(win-load (% (+ (dec *index*) (length *pcbs*)) (length *pcbs*)))
	(stop-route))

(defun action-next ()
	(win-load (% (+ (inc *index*) (length *pcbs*)) (length *pcbs*)))
	(stop-route))

(defun action-scale-up ()
	(setq *zoom* (max (min (+ *zoom* 1.0) +max_zoom) +min_zoom))
	(win-zoom))

(defun action-scale-down ()
	(setq *zoom* (max (min (- *zoom* 1.0) +max_zoom) +min_zoom))
	(win-zoom))

(defun action-show-all ()
	(radio-select *layer_toolbar* (setq *show* 0))
	(win-show))

(defun action-show1 ()
	(radio-select *layer_toolbar* (setq *show* 1))
	(win-show))

(defun action-show2 ()
	(radio-select *layer_toolbar* (setq *show* 2))
	(win-show))

(defun action-show3 ()
	(radio-select *layer_toolbar* (setq *show* 3))
	(win-show))

(defun action-show4 ()
	(radio-select *layer_toolbar* (setq *show* 4))
	(win-show))

(defun action-mode-normal ()
	(radio-select *mode_toolbar* (setq *mode* 0))
	(win-show))

(defun action-mode-gerber ()
	(radio-select *mode_toolbar* (setq *mode* 1))
	(win-show))
