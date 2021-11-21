;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* nil))

(defun action-reset ()
	(route))

(defun action-res-spinner ()
	(setq *grid_res* (get :value res_spinner)))

(defun action-vias-spinner ()
	(setq *vias_cost* (get :value vias_spinner)))

(defun action-quant-spinner ()
	(setq *quant* (get :value quant_spinner)))

(defun action-flood-spinner ()
	(setq *flood_range* (get :value flood_spinner)))

(defun action-even-spinner ()
	(setq *even_range* (get :value even_spinner)))

(defun action-odd-spinner ()
	(setq *odd_range* (get :value odd_spinner)))

(defun action-prev ()
	(win-load (% (+ (dec *index*) (length *pcbs*)) (length *pcbs*)))
	(stop-route))

(defun action-next ()
	(win-load (% (+ (inc *index*) (length *pcbs*)) (length *pcbs*)))
	(stop-route))

(defun action-zoom-in ()
	(setq *zoom* (max (min (+ *zoom* 1.0) +max_zoom) +min_zoom))
	(win-zoom))

(defun action-zoom-out ()
	(setq *zoom* (max (min (- *zoom* 1.0) +max_zoom) +min_zoom))
	(win-zoom))

(defun action-show-all ()
	(setq *show* -1)
	(win-show))

(defun action-show1 ()
	(setq *show* 0)
	(win-show))

(defun action-show2 ()
	(setq *show* 1)
	(win-show))

(defun action-show3 ()
	(setq *show* 2)
	(win-show))

(defun action-show4 ()
	(setq *show* 3)
	(win-show))

(defun action-mode-normal ()
	(setq *mode* 0)
	(win-show))

(defun action-mode-gerber ()
	(setq *mode* 1)
	(win-show))
