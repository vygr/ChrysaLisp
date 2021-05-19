;cursor left
(defun action-left ()
	(. text_buf :left)
	(clear-underlay) (refresh))
;cursor right
(defun action-right ()
	(. text_buf :right)
	(clear-underlay) (refresh))
;cursor up
(defun action-up ()
	(. text_buf :up)
	(clear-underlay) (refresh))
;cursor down
(defun action-down ()
	(. text_buf :down)
	(clear-underlay) (refresh))
;line break
(defun action-break ()
	(. text_buf :cut anchor_x anchor_y)
	(. text_buf :break)
	(clear-underlay) (refresh))
;backspace
(defun action-backspace ()
	(when (eql (. text_buf :cut anchor_x anchor_y) "")
		(. text_buf :backspace))
	(clear-underlay) (refresh))
;delete
(defun action-delete ()
	(when (eql (. text_buf :cut anchor_x anchor_y) "")
		(. text_buf :delete))
	(clear-underlay) (refresh))
;tab
(defun action-tab ()
	(. text_buf :cut anchor_x anchor_y)
	(bind '(x y) (. text_buf :get_cursor))
	(defq tab_width (get :tab_width text_buf))
	(. text_buf :insert (pad "" (- tab_width (% x tab_width)) "        "))
	(clear-underlay) (refresh))
;undo
(defun action-undo ()
	(. text_buf :undo)
	(clear-underlay) (refresh))
;redo
(defun action-redo ()
	(. text_buf :redo)
	(clear-underlay) (refresh))
;save
(defun action-save ()
	(. text_buf :file_save current_file))
;cut
(defun action-cut ()
	(clipboard-put (. text_buf :cut anchor_x anchor_y))
	(clear-underlay) (refresh))
;copy
(defun action-copy ()
	(clipboard-put (. text_buf :copy anchor_x anchor_y))
	(clear-underlay) (refresh))
;paste
(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(. text_buf :cut anchor_x anchor_y)
		(. text_buf :paste data)
		(clear-underlay) (refresh)))
;minimise
(defun action-minimise ()
		(vdu-resize vdu_min_width vdu_min_height))
;maximise
(defun action-maximise ()
		(vdu-resize vdu_max_width vdu_max_height))
