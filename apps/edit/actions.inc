;module
(env-push)

(defmacro undoable (&rest body)
	`(progn
		(. (defq buffer *current_buffer*) :push_undo
			(list :mark (defq mark (. buffer :next_mark)))
			(list :cursor *cursor_x* *cursor_y*))
		~body
		(. buffer :push_undo (list :mark mark))))

(defun selection? ()
	(or (/= *anchor_x* *cursor_x*) (/= *anchor_y* *cursor_y*)))

(defun sort-selection ()
	(defq sx *cursor_x* sy *cursor_y* sx1 *anchor_x* sy1 *anchor_y*)
	(if (> sy sy1)
		(defq sx (logxor sx sx1) sx1 (logxor sx sx1) sx (logxor sx sx1)
			sy (logxor sy sy1) sy1 (logxor sy sy1) sy (logxor sy sy1)))
	(and (= sy sy1) (> sx sx1)
		(defq sx (logxor sx sx1) sx1 (logxor sx sx1) sx (logxor sx sx1)))
	(list sx sy sx1 sy1))

(defun select-lines ()
	(bind '(sx sy sx1 sy1) (sort-selection))
	(if (or (> sx1 0) (= sy sy1)) (setq sy1 (inc sy1)))
	(list sy sy1))

(defun select-word (buffer)
	(defq line (. buffer :get_text_line *cursor_y*)
		x (some! *cursor_x* 0 nil (#
			(if (find %0 " ()") (inc _))) (list line))
		x1 (some! *cursor_x* -1 nil (#
			(if (find %0 " ()") _)) (list line)))
	(unless x (setq x 0))
	(unless x1 (setq x1 (length line)))
	(list x x1))

(defun select-paragraph (buffer)
	(defq y (some! *cursor_y* 0 nil
				(# (if (= (length %0) 0) (inc _)))
			(list (. buffer :get_text_lines)))
		y1 (some! *cursor_y* (bind '(w h) (. buffer :get_size)) nil
				(# (if (= (length %0) 0) _))
			(list (. buffer :get_text_lines))))
	(unless y (setq y 0))
	(unless y1 (setq y1 h))
	(list y y1))

(defun action-insert (key)
	(. *current_buffer* :cut *anchor_x* *anchor_y*)
	(. *current_buffer* :insert (char key))
	(clear-selection) (refresh))

(defun action-left ()
	(. *current_buffer* :left)
	(clear-selection) (refresh))

(defun action-right ()
	(. *current_buffer* :right)
	(clear-selection) (refresh))

(defun action-up ()
	(. *current_buffer* :up)
	(clear-selection) (refresh))

(defun action-down ()
	(. *current_buffer* :down)
	(clear-selection) (refresh))

(defun action-left-bracket ()
	(when (bind '(x y) (. *current_buffer* :left_bracket))
		(. *current_buffer* :set_cursor x y)
		(clear-selection) (refresh)))

(defun action-right-bracket ()
	(when (bind '(x y) (. *current_buffer* :right_bracket))
		(. *current_buffer* :set_cursor x y)
		(clear-selection) (refresh)))

(defun action-left-select ()
	(unless *shift_select*
		(setq *anchor_x* *cursor_x* *anchor_y* *cursor_y* *shift_select* t))
	(. *current_buffer* :left)
	(refresh))

(defun action-right-select ()
	(unless *shift_select*
		(setq *anchor_x* *cursor_x* *anchor_y* *cursor_y* *shift_select* t))
	(. *current_buffer* :right)
	(refresh))

(defun action-up-select ()
	(unless *shift_select*
		(setq *anchor_x* *cursor_x* *anchor_y* *cursor_y* *shift_select* t))
	(. *current_buffer* :up)
	(refresh))

(defun action-down-select ()
	(unless *shift_select*
		(setq *anchor_x* *cursor_x* *anchor_y* *cursor_y* *shift_select* t))
	(. *current_buffer* :down)
	(refresh))

(defun action-left-tab ()
	(undoable
		(defq tab_width (. buffer :get_tab_width))
		(bind '(y y1) (select-lines))
		(each! y y1 (lambda (line)
				(defq idx -1)
				(while (and
					(< (setq idx (inc idx)) (length line))
					(< idx tab_width)
					(eql (elem idx line) " ")))
				(if (> idx 0)
					(.-> buffer (:set_cursor 0 _) (:delete (min tab_width idx)))))
			(list (. buffer :get_text_lines)))
		(. buffer :set_cursor *cursor_x* *cursor_y*))
	(refresh))

(defun action-right-tab ()
	(undoable
		(defq tab (pad "" (. buffer :get_tab_width) "        "))
		(bind '(y y1) (select-lines))
		(each! y y1 (lambda (line)
				(unless (eql line "")
					(.-> buffer
						(:set_cursor 0 _)
						(:insert tab))))
			(list (. buffer :get_text_lines)))
		(. buffer :set_cursor *cursor_x* *cursor_y*))
	(refresh))

(defun action-break ()
	(undoable
		(. buffer :cut *anchor_x* *anchor_y*)
		(bind '(x y) (.-> buffer :break :get_cursor))
		(defq line (. buffer :get_text_line (dec y))
			indent (some (# (unless (eql %0 (ascii-char +char_space)) _)) line))
		(unless indent (setq indent (length line)))
		(if (> indent 0) (. buffer :insert (pad "" indent "        "))))
	(clear-selection) (refresh))

(defun action-reflow ()
	(undoable
		(bind '(y y1) (select-paragraph buffer))
		(each (lambda (line) (.-> buffer (:insert line) :break))
			(. (. buffer :get_syntax) :text_flow
				(split (.-> buffer (:set_cursor 0 y) (:cut 0 y1)) (const (cat " " (ascii-char +char_lf))))
				(. buffer :get_wrap_width)))
		(bind '(x y) (. buffer :get_cursor))
		(bind '(x y) (. buffer :constrain x (inc y)))
		(. buffer :set_cursor x y))
	(clear-selection) (refresh))

(defun action-backspace ()
	(when (eql (. *current_buffer* :cut *anchor_x* *anchor_y*) "")
		(. *current_buffer* :backspace))
	(clear-selection) (refresh))

(defun action-delete ()
	(when (eql (. *current_buffer* :cut *anchor_x* *anchor_y*) "")
		(. *current_buffer* :delete))
	(clear-selection) (refresh))

(defun action-tab ()
	(cond
		((= *anchor_y* *cursor_y*)
			(undoable
				(defq tab_width (. buffer :get_tab_width))
				(. buffer :cut *anchor_x* *anchor_y*)
				(bind '(x y) (. buffer :get_cursor))
				(. buffer :insert (pad "" (- tab_width (% x tab_width)) "        ")))
			(clear-selection) (refresh))
		((action-right-tab))))

(defun action-undo ()
	(. *current_buffer* :undo)
	(clear-selection) (refresh))

(defun action-redo ()
	(. *current_buffer* :redo)
	(clear-selection) (refresh))

(defun action-rewind ()
	(while (> (length (get :undo_stack *current_buffer*)) 0)
		(. *current_buffer* :undo))
	(clear-selection) (refresh))

(defun trim-buffer (*current_buffer*)
	(when (> (bind '(w h) (. *current_buffer* :get_size)) 0)
		(undoable
			;trim front
			(while (and (> (bind '(w h) (. buffer :get_size)) 0)
						(eql (. buffer :get_text_line 0) ""))
				(.-> buffer (:set_cursor 0 0) :delete))
			;trim end
			(while (and (> (bind '(w h) (. buffer :get_size)) 0)
						(eql (. buffer :get_text_line (dec h)) ""))
				(.-> buffer (:set_cursor 0 (dec h)) :backspace))
			;trim lines
			(each! 0 h (lambda (line)
					(unless (eql (defq trimed_line (trim-end line)) line)
						(.-> buffer
							(:set_cursor (length trimed_line) _)
							(:delete (- (length line) (length trimed_line))))))
				(list (. buffer :get_text_lines)))
			(bind '(x y) (. buffer :constrain *cursor_x* *cursor_y*))
		(. buffer :set_cursor x y)))
	*current_buffer*)

(defun action-trim ()
	(trim-buffer *current_buffer*)
	(clear-selection) (refresh))

(defun action-save ()
	(when *current_file*
		(. (trim-buffer *current_buffer*) :file_save *current_file*)
		(clear-selection) (refresh)))

(defun action-save-all ()
	(. *meta_map* :each (lambda (file (_ _ _ _ _ _ _ buffer))
		(if file (. (trim-buffer buffer) :file_save file))))
	(clear-selection) (refresh))

(defun action-cut ()
	(clipboard-put (. *current_buffer* :cut *anchor_x* *anchor_y*))
	(clear-selection) (refresh))

(defun action-copy ()
	(clipboard-put (. *current_buffer* :copy *anchor_x* *anchor_y*))
	(refresh))

(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(undoable
			(. buffer :cut *anchor_x* *anchor_y*)
			(. buffer :paste data))
		(clear-selection) (refresh)))

(defun action-select-all ()
	(bind '(w h) (. *current_buffer* :get_size))
	(setq *anchor_x* 0 *anchor_y* 0 *shift_select* t)
	(. *current_buffer* :set_cursor 0 h)
	(refresh))

(defun action-select-word ()
	(bind '(x x1) (select-word *current_buffer*))
	(setq *anchor_x* x *anchor_y* *cursor_y* *shift_select* t)
	(. *current_buffer* :set_cursor x1 *cursor_y*)
	(refresh))

(defun action-select-line ()
	(setq *anchor_x* 0 *anchor_y* *cursor_y* *shift_select* t)
	(. *current_buffer* :set_cursor 0 (inc *cursor_y*))
	(refresh))

(defun action-select-paragraph ()
	(bind '(y y1) (select-paragraph *current_buffer*))
	(setq *anchor_x* 0 *anchor_y* y *shift_select* t)
	(. *current_buffer* :set_cursor 0 y1)
	(refresh))

(defun action-select-block ()
	(when (bind '(x y) (. (defq buffer *current_buffer*) :left_bracket))
		(when (bind '(x1 y1) (. buffer :right_bracket))
			(setq *anchor_x* x *anchor_y* y *shift_select* t)
			(. buffer :set_cursor (inc x1) y1)
			(refresh))))

(defun action-cut-word ()
	(action-select-word)
	(action-cut))

(defun action-cut-line ()
	(action-select-line)
	(action-cut))

(defun action-cut-paragraph ()
	(action-select-paragraph)
	(action-cut))

(defun action-cut-block ()
	(action-select-block)
	(action-cut))

(defun action-minimise ()
	(vdu-resize +vdu_min_width +vdu_min_height))

(defun action-maximise ()
	(vdu-resize +vdu_max_width +vdu_max_height))

(defun action-close ()
	(setq *running* nil))

(defun action-layout ()
	;user window resize
	(apply window-resize (. *vdu* :max_size)))

(defun action-xscroll ()
	;user xscroll bar
	(setq *scroll_x* (get :value *xslider*))
	(load-display))

(defun action-yscroll ()
	;user yscroll bar
	(setq *scroll_y* (get :value *yslider*))
	(load-display))

(defun action-tree-action ()
	;any tree mutate action
	(.-> (penv (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
		:layout :dirty_all))

(defun action-file-leaf-action ()
	;load up the file selected
	(. *open_tree* :add_route (defq file (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(select-node file)
	(populate-vdu file))

(defun action-open-leaf-action ()
	;switch to the file selected
	(. *file_tree* :add_route (defq file (. *open_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(select-node file)
	(populate-vdu file))

(defun action-new ()
	(when (and (not (eql (defq file (get :clear_text *name_text*)) ""))
			(= (age file) 0))
		;file does not exist
		(save "" file)
		(def *name_text* :cursor 0 :clear_text "")
		(.-> *name_text* :layout :dirty_all)
		(. *open_tree* :add_route file)
		(. *file_tree* :add_route file)
		(select-node file)
		(populate-vdu file)))

(defun action-next-buffer ()
	(defq live_buffers (list))
	(. *meta_map* :each (lambda (k v) (push live_buffers k)))
	(defq idx (inc (find *current_file* live_buffers)))
	(if (>= idx (length live_buffers)) (setq idx 0))
	(select-node (elem idx live_buffers))
	(populate-vdu (elem idx live_buffers)))

(defun action-prev-buffer ()
	(defq live_buffers (list))
	(. *meta_map* :each (lambda (k v) (push live_buffers k)))
	(defq idx (dec (find *current_file* live_buffers)))
	(if (< idx 0) (setq idx (dec (length live_buffers))))
	(select-node (elem idx live_buffers))
	(populate-vdu (elem idx live_buffers)))

(defun action-find-down ()
	(unless (eql (defq pattern (get :clear_text *find_text*)) "")
		(when (> (length (defq found (. *current_buffer* :find pattern))) 0)
			(when (defq next (some (# (bind '(x y) %0)
					(if (or (> y *cursor_y*)
							(and (= y *cursor_y*) (>= x *cursor_x*)))
						%0)) found))
				(bind '(x y) next)
				(setq *anchor_x* x *anchor_y* y)
				(. *current_buffer* :set_cursor (+ x (length pattern)) y)
				(refresh)))))

(defun action-find-up ()
	(unless (eql (defq pattern (get :clear_text *find_text*)) "")
		(when (> (length (defq found (. *current_buffer* :find pattern))) 0)
			(when (defq next (some-rev (# (bind '(x y) %0)
					(if (or (< y *cursor_y*)
							(and (= y *cursor_y*) (< x *cursor_x*)))
						%0)) found))
				(bind '(x y) next)
				(setq *anchor_x* (+ x (length pattern)) *anchor_y* y)
				(. *current_buffer* :set_cursor x y)
				(refresh)))))

(defun action-set-find-text ()
	(if (selection?)
		(when (> (length (defq text
				(split (. *current_buffer* :copy *anchor_x* *anchor_y*)
					(ascii-char +char_lf)))) 0)
			(setq text (elem 0 text))
			(set *find_text* :clear_text text :cursor (length text))
			(.-> *find_text* :layout :dirty))))

(defun action-replace ()
	(unless (or (eql (defq pattern (get :clear_text *find_text*)) "")
				(eql (defq replace (get :clear_text *replace_text*)) pattern))
		(when (eql (. *current_buffer* :copy *anchor_x* *anchor_y*) pattern)
			(undoable
				(. buffer :cut *anchor_x* *anchor_y*)
				(. buffer :insert replace))
			(refresh))
		(action-find-down)))

(defun action-replace-all ()
	(unless (or (eql (defq pattern (get :clear_text *find_text*)) "")
				(eql (defq replace (get :clear_text *replace_text*)) pattern))
		(when (> (length (defq found (. *current_buffer* :find pattern))) 0)
			(undoable
				(each-rev (lambda ((x y))
						(.-> buffer (:set_cursor x y) (:cut (+ x (length pattern)) y))
						(unless (eql replace "") (. buffer :insert replace)))
					found))
			(refresh))))

(defun action-to-lower ()
	(when (selection?)
		(undoable
			(. buffer :paste (to-lower (. buffer :cut *anchor_x* *anchor_y*))))
		(refresh)))

(defun action-to-upper ()
	(when (selection?)
		(undoable
			(. buffer :paste (to-upper (. buffer :cut *anchor_x* *anchor_y*))))
		(refresh)))

(defun action-ordered ()
	(when (selection?)
		(undoable
			(bind '(y y1) (select-lines))
			(setq *anchor_x* 0 *anchor_y* y)
			(. buffer :set_cursor 0 y1)
			(defq text (cat (join
				(sort cmp (split (. buffer :cut 0 y) (ascii-char +char_lf)))
				(ascii-char +char_lf)) (ascii-char +char_lf)))
			(. buffer :paste text))
		(refresh)))

(defun action-ordered-unique ()
	(when (selection?)
		(undoable
			(bind '(y y1) (select-lines))
			(setq *anchor_x* 0 *anchor_y* y)
			(. buffer :set_cursor 0 y1)
			(defq text (cat (join
				(reduce (# (if (or (<= (length %0) 0)
								(not (eql (elem -2 %0) %1))) (push %0 %1) %0))
				(sort cmp (split (. buffer :cut 0 y) (ascii-char +char_lf)))
				(list)) (ascii-char +char_lf)) (ascii-char +char_lf)))
			(. buffer :paste text))
		(refresh)))

(defun action-close-buffer ()
	(when *current_file*
		(each (# (. %0 :sub)) (. *open_tree* :children))
		(. *meta_map* :erase *current_file*)
		(when (defq idx (find *current_file* *open_files*))
			(setq *open_files* (erase *open_files* idx (inc idx))))
		(defq file (first *open_files*))
		(populate-open-tree)
		(populate-vdu file)
		(select-node file)))

(defq
event_map (xmap-kv
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_close_buffer action-close-buffer
	+event_save action-save
	+event_save_all action-save-all
	+event_new action-new
	+event_next action-next-buffer
	+event_prev action-prev-buffer
	+event_find_down action-find-down
	+event_find_up action-find-up
	+event_replace action-replace
	+event_replace_all action-replace-all
	+event_undo action-undo
	+event_redo action-redo
	+event_rewind action-rewind
	+event_cut action-cut
	+event_copy action-copy
	+event_paste action-paste
	+event_tab_left action-left-tab
	+event_tab_right action-right-tab
	+event_reflow action-reflow
	+event_layout action-layout
	+event_xscroll action-xscroll
	+event_yscroll action-yscroll
	+event_file_leaf_action action-file-leaf-action
	+event_tree_action action-tree-action
	+event_open_leaf_action action-open-leaf-action)

key_map (xmap-kv
	0x40000050 action-left
	0x4000004f action-right
	0x40000052 action-up
	0x40000051 action-down
	+char_lf action-break
	+char_cr action-break
	+char_backspace action-backspace
	+char_delete action-delete
	+char_tab action-tab)

key_map_shift (xmap-kv
	0x40000050 action-left-select
	0x4000004f action-right-select
	0x40000052 action-up-select
	0x40000051 action-down-select
	+char_tab action-left-tab)

key_map_control (xmap-kv
	(ascii-code "a") action-select-all
	(ascii-code "b") action-select-block
	(ascii-code "B") action-cut-block
	(ascii-code "w") action-select-word
	(ascii-code "W") action-cut-word
	(ascii-code "l") action-select-line
	(ascii-code "L") action-cut-line
	(ascii-code "p") action-select-paragraph
	(ascii-code "P") action-cut-paragraph
	(ascii-code "f") action-set-find-text
	(ascii-code "r") action-replace
	(ascii-code "R") action-replace-all
	(ascii-code "z") action-undo
	(ascii-code "Z") action-redo
	(ascii-code "s") action-save
	(ascii-code "S") action-save-all
	(ascii-code "t") action-trim
	(ascii-code "x") action-cut
	(ascii-code "c") action-copy
	(ascii-code "v") action-paste
	(ascii-code "-") action-minimise
	(ascii-code "=") action-maximise
	(ascii-code "n") action-next-buffer
	(ascii-code "N") action-prev-buffer
	(ascii-code "q") action-reflow
	(ascii-code "9") action-left-bracket
	(ascii-code "0") action-right-bracket
	(ascii-code "u") action-to-upper
	(ascii-code "U") action-to-lower
	(ascii-code "o") action-ordered
	(ascii-code "O") action-ordered-unique))

;module
(export (penv)
	event_map key_map key_map_shift key_map_control
	action-insert action-select-word action-select-line action-select-paragraph)
(env-pop)
