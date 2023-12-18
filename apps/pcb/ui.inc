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
	(setq *show* -1)
	(. *layer_toolbar* :set_selected 0)
	(win-show))

(defun action-show-1 ()
	(setq *show* 0)
	(. *layer_toolbar* :set_selected 1)
	(win-show))

(defun action-show-2 ()
	(setq *show* 1)
	(. *layer_toolbar* :set_selected 2)
	(win-show))

(defun action-show-3 ()
	(setq *show* 2)
	(. *layer_toolbar* :set_selected 3)
	(win-show))

(defun action-show-4 ()
	(setq *show* 3)
	(. *layer_toolbar* :set_selected 4)
	(win-show))

(defun action-mode-normal ()
	(setq *mode* 0)
	(. *mode_toolbar* :set_selected 0)
	(win-show))

(defun action-mode-gerber ()
	(setq *mode* 1)
	(. *mode_toolbar* :set_selected 1)
	(win-show))

(defun action-layer ()
	(setq *show* (dec (. *layer_toolbar* :get_selected)))
	(win-show))

(defun action-mode ()
	(setq *mode* (. *mode_toolbar* :get_selected))
	(win-show))
