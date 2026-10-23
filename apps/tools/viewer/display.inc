;the Viewer's and Hexview's display, the Editor's and the file meta data

(import "././edit/display.inc")

(defun select-node (file)
	;highlight and show the selected file
	(. *file_selector* :select_node file))

(defun update-meta-data ()
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy ax ay) (. *edit* :get_cursor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(scatter *meta_map*
		:file (str *current_file*)
		:find (. *find_text* :get_text))
	(scatter (.-> *meta_map* (:find :files) (:find (str *current_file*)))
		:cx cx :cy cy :ax ax :ay ay :sx sx :sy sy :buffer buffer))
