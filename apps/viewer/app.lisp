;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/text/syntax.inc")

(structure '+event 0
	(byte 'close+ 'max+ 'min+)
	(byte 'layout+ 'scroll+)
	(byte 'tree_button+ 'file_button+))

(defq vdu_min_width 16 vdu_min_height 16
	vdu_max_width 120 vdu_max_height 50
	vdu_width 80 vdu_height 50
	text_buf nil syntax (Syntax) scroll_positions (xmap 101)
	current_file nil current_button nil)

(ui-window mywindow (:color +argb_grey2+)
	(ui-title-bar _ "Viewer" (0xea19 0xea1b 0xea1a) +event_close+)
	(ui-flow _ (:flow_flags +flow_right_fill+ :font *env_terminal_font*)
		(ui-scroll tree_scroll +scroll_flag_vertical+ nil
			(ui-tree tree (:action_event +event_file_button+ :min_width 0 :color +argb_white+)))
		(ui-flow _ (:flow_flags +flow_left_fill+)
			(component-connect (ui-slider slider) +event_scroll+)
			(ui-vdu vdu (:min_width vdu_width :min_height vdu_height
				:vdu_width vdu_width :vdu_height vdu_height
				:ink_color +argb_white+)))))

(defun all-files (dir)
	;all files from dir downwards
	(defq dirs (list) files (list))
	(each! 0 -1 (lambda (f d)
		(unless (or (starts-with "." f) (starts-with "obj" f))
			(push (if (eql "4" d) dirs files) (cat dir "/" f))))
		(unzip (split (pii-dirlist dir) ",") (list (list) (list))))
	(each (lambda (d)
		(setq files (cat files (all-files d)))) dirs)
	files)

(defun set-slider (file)
	;set slider values for this file
	(defq scroll_max (max 0 (- (length text_buf) vdu_height))
		scroll_position (min (. scroll_positions :find file) scroll_max))
	(def (. slider :dirty) :maximum scroll_max :portion vdu_height :value scroll_position)
	scroll_position)

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(. syntax :set_state :text)
	(setq text_buf (list) current_file file)
	(each-line (lambda (line)
		(push text_buf (. syntax :colorise
			(if (> (length line) 0)
				(apply cat (map (# (if (eql %0 (ascii-char 9)) "    " %0)) line))
				line))))
		(file-stream file))
	(. vdu :load text_buf 0 (set-slider file) 0 -1))

(defun populate-tree ()
	;load up the file tree and the first file
	(defq all_src_files
		(map (# (slice 2 -1 %0)) (sort cmp (filter (# (or
			(ends-with ".vp" %0)
			(ends-with ".inc" %0)
			(ends-with ".lisp" %0))) (all-files ".")))))
	(each (# (. tree :add_route %0)) all_src_files)
	(each (# (. scroll_positions :insert %0 0)) all_src_files)
	(populate-vdu (elem 0 all_src_files)))

(defun window-resize (w h)
	;layout the window and size the vdu to fit
	(setq vdu_width w vdu_height h)
	(set vdu :vdu_width w :vdu_height h :min_width w :min_height h)
	(bind '(x y) (. vdu :get_pos))
	(bind '(w h) (. vdu :pref_size))
	(set vdu :min_width vdu_min_width :min_height vdu_min_height)
	(view-change vdu x y w h)
	(. vdu :load text_buf 0 (set-slider current_file) 0 -1))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(setq vdu_width w vdu_height h)
	(set vdu :vdu_width w :vdu_height h :min_width w :min_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. mywindow :get_pos) (. mywindow :pref_size))))
	(set vdu :min_width vdu_min_width :min_height vdu_min_height)
	(. mywindow :change_dirty x y w h)
	(. vdu :load text_buf 0 (set-slider current_file) 0 -1))

(defun main ()
	(populate-tree)
	(bind '(w h) (. tree :pref_size))
	(view-change tree 0 0 (def tree_scroll :min_width w) h)
	(bind '(x y w h) (apply view-locate (. (component-connect mywindow +event_layout+) :pref_size)))
	(gui-add (view-change mywindow x y w h))
	(. vdu :load text_buf 0 0 0 -1)
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +event_close+)
			nil)
		((= id +event_layout+)
			;user window resize
			(apply window-resize (. vdu :max_size)))
		((= id +event_min+)
			;min button
			(vdu-resize vdu_min_width vdu_min_height))
		((= id +event_max+)
			;max button
			(vdu-resize vdu_max_width vdu_max_height))
		((= id +event_scroll+)
			;user scroll bar
			(defq scroll_position (get :value slider))
			(. scroll_positions :insert current_file scroll_position)
			(. vdu :load text_buf 0 scroll_position 0 -1))
		((= id +event_file_button+)
			;load up the file selected
			(if current_button
				(def (. current_button :dirty) :color +argb_white+))
			(setq current_button (. mywindow :find_id (get-long msg ev_msg_action_source_id)))
			(def (. current_button :dirty) :color +argb_grey12+)
			(populate-vdu (. tree :get_route current_button)))
		(t (. mywindow :event msg))))
	(. mywindow :hide))
