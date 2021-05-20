;cursor left
(defun action-left ()
	(. text_buf :left)
	(clear-selection) (refresh))
;cursor right
(defun action-right ()
	(. text_buf :right)
	(clear-selection) (refresh))
;cursor up
(defun action-up ()
	(. text_buf :up)
	(clear-selection) (refresh))
;cursor down
(defun action-down ()
	(. text_buf :down)
	(clear-selection) (refresh))
;line break
(defun action-break ()
	(. text_buf :cut anchor_x anchor_y)
	(. text_buf :break)
	(clear-selection) (refresh))
;backspace
(defun action-backspace ()
	(when (eql (. text_buf :cut anchor_x anchor_y) "")
		(. text_buf :backspace))
	(clear-selection) (refresh))
;delete
(defun action-delete ()
	(when (eql (. text_buf :cut anchor_x anchor_y) "")
		(. text_buf :delete))
	(clear-selection) (refresh))
;tab
(defun action-tab ()
	(. text_buf :cut anchor_x anchor_y)
	(bind '(x y) (. text_buf :get_cursor))
	(defq tab_width (get :tab_width text_buf))
	(. text_buf :insert (pad "" (- tab_width (% x tab_width)) "        "))
	(clear-selection) (refresh))
;undo
(defun action-undo ()
	(. text_buf :undo)
	(clear-selection) (refresh))
;redo
(defun action-redo ()
	(. text_buf :redo)
	(clear-selection) (refresh))
;trim
(defun action-trim ()
	(. text_buf :trim)
	(clear-selection) (refresh))
;save
(defun action-save ()
	(action-trim)
	(. text_buf :file_save current_file))
;cut
(defun action-cut ()
	(clipboard-put (. text_buf :cut anchor_x anchor_y))
	(clear-selection) (refresh))
;copy
(defun action-copy ()
	(clipboard-put (. text_buf :copy anchor_x anchor_y))
	(clear-selection) (refresh))
;paste
(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(. text_buf :cut anchor_x anchor_y)
		(. text_buf :paste data)
		(clear-selection) (refresh)))
;select all
(defun action-select-all ()
		(bind '(_ y) (. text_buf :get_size))
		(setq anchor_x 0 anchor_y 0)
		(. text_buf :set_cursor 0 y)
		(create-selection) (refresh))
;minimise
(defun action-minimise ()
		(vdu-resize vdu_min_width vdu_min_height))
;maximise
(defun action-maximise ()
		(vdu-resize vdu_max_width vdu_max_height))
