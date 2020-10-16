(defmacro-bind find-function (fn)
	`(let ()
		(bind '(_ _ _ buffer position) current_text)
		(bind '(ox oy cx cy sx) (elem +text_position+ current_text))
		,fn (cursor-visible)
		(elem-set +text_position+ current_text (list ox oy cx cy sx))			
		(elem-set +text_position+ current_text (list ox oy cx cy sx))))

(defun-bind find-in-buffer (ss)
	(unless (eql ss "")
		(each (lambda (ln) (when (not (eql nil (defq found (substr ss ln))))
			(defq y _) (each (lambda (x) (push find_list (list x y))) found))) buffer)
		(setq find_index 0)))

(defun-bind find-start ()
	;if there is a previous search, clear and conduct a new one.
	(when (/= (length find_list) 0) (setq find_list (list) find_index 0))
	;Searches are placed within the (find:<string>) command. Just take the slice.
	(find-in-buffer (setq find_textfield (get :text textfield)))
	(cond
		((> (defq fl_len (length find_list)) 0)
			(bind '(fx fy) (first find_list))
			(elem-set 2 (elem +text_position+ current_text) fx) (elem-set 3 (elem +text_position+ current_text) fy)
			(setq sb_line_col_message (str " - " (str (inc find_index)) " of " (str (length find_list)) " Matches. "))
			(set sb_line_col :text (cat "Line " (str fy) ", Column " (str fx) sb_line_col_message))
			(view-layout status_bar)
			(view-dirty sb_line_col))
		((eql (get :text textfield) "") (setq sb_line_col_message "")
			(view-dirty sb_line_col))
		(t 
			(setq sb_line_col_message " - No Matches Found")
			(set sb_line_col :text (cat "Line " (str cy) ", Column " (str cx) sb_line_col_message))
			(view-layout sb_line_col)
			(view-layout status_bar)
			(view-dirty sb_line_col))))

(defun-bind find-next ()
	(cond 
		((or (not (eql (get :text textfield) find_textfield)) (= (length find_list) 0)) (find-start))
		(t 
			(if (= find_index (dec (defq fl_len (length find_list))))
				(setq find_index 0) (setq find_index (inc find_index)))
			(bind '(fx fy) (elem find_index find_list))
			(setq cx fx cy fy)
			(setq sb_line_col_message (str " - " (str (inc find_index)) " of " (str (length find_list)) " Matches. "))
			(set sb_line_col :text (cat "Line " (str fy) ", Column " (str fx) sb_line_col_message))
			(view-layout sb_line_col))))

(defun-bind find-prev ()
	(cond 
		((or (not (eql (get :text textfield) find_textfield)) (= (length find_list) 0)) (find-start))
		(t 
			(if (= find_index 0) 
				(setq find_index (dec (length find_list))) (setq find_index (dec find_index)))
			(bind '(fx fy) (elem find_index find_list))
			(setq cx fx cy fy)
			(setq sb_line_col_message (str " - " (str (inc find_index)) " of " (str (length find_list)) " Matches. "))
			(set sb_line_col :text (cat "Line " (str fy) ", Column " (str fx) sb_line_col_message))
			(view-layout sb_line_col))))
