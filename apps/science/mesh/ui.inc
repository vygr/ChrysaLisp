;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-canvas (size)
	;the size of picture that is wanted. No canvas is made of it here: a
	;window that is being dragged says a new size many times a second, and
	;the frame timer makes a canvas of the last of them when the next frame
	;is to be drawn, (new-canvas). The picture on show still fills what it
	;is in, that may be a new size, and is drawn again now it is laid out,
	;if nothing is turning there is no next frame
	(bind '(w h) (. *main_backdrop* :get_size))
	(. (ifn *shown* *main_widget*) :change 0 0 w h)
	(. *window* :layout)
	(setq *want_size* size *dirty* :t))

(defun set-size (size)
	(def *main_backdrop* :want_width size :want_height size)
	(. *window* :fit)
	(set-canvas size))

(defun action-minimise ()
	(set-size +min_size))

(defun action-maximise ()
	(set-size +max_size))

(defun action-gpu ()
	;the faces by the GPU, or by the nodes, as the buttons say
	(setq *use_gpu* (= (. *gpu_toolbar* :get_selected) 1) *dirty* :t))

(defun action-gpu-key ()
	;the g key, the other of the two
	(. *gpu_toolbar* :set_selected (if *use_gpu* 0 1))
	(action-gpu))

(defun action-layout ()
	(bind '(w h) (. *main_backdrop* :get_size))
	(set-canvas (min w h)))

(defun action-close ()
	(setq *running* :nil))

(defun action-mode ()
	(defq radio_col (canvas-brighter (get :paper *main_toolbar*))
		button (first (. *main_toolbar* :children)))
	(undef (. button :dirty) :paper)
	(if (setq *dirty* :t *render_mode* (not *render_mode*))
		(def button :paper radio_col)))

(defun action-auto ()
	(defq radio_col (canvas-brighter (get :paper *main_toolbar*))
		button (second (. *main_toolbar* :children)))
	(undef (. button :dirty) :paper)
	(if (setq *auto_mode* (not *auto_mode*))
		(def button :paper radio_col)))

(defun action-xrot ()
	(if *auto_mode* (action-auto))
	(setq *rotx* (get-rot *xrot_slider*) *dirty* :t))

(defun action-yrot ()
	(if *auto_mode* (action-auto))
	(setq *roty* (get-rot *yrot_slider*) *dirty* :t))

(defun action-zrot ()
	(if *auto_mode* (action-auto))
	(setq *rotz* (get-rot *zrot_slider*) *dirty* :t))

(defun action-style ()
	(def (. *main_backdrop* :dirty) :style
			(elem-get '(:plain :grid :axis) (. *style_toolbar* :get_selected))))
