(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close button))

(defq keys (list) vals (list) vdu_height 40 text_buf (list ""))

(defun populate-help ()
	(defq state :t vdu_width 1 k (list) v (list))
	(each-line (lambda (_)
		(when (/= 0 (length (defq s (split (setq _ (trim-end _ (ascii-char 13))) " "))))
			(defq f (elem-get 0 s))
			(cond
				(state (cond
					((eql f "###") (push k (sym (cat (elem-get 1 s) " " (elem-get 2 s)))) (push v ""))
					((eql f "```code") (setq state :nil))))
				((eql f "```") (setq state :t))
				(:t (elem-set -2 v (cat (elem-get -2 v) _ (ascii-char 10))))))) (file-stream "docs/VP_CLASSES.md"))
	(each (lambda (k v)
		(when (/= 0 (length v))
			(defq _ (split k ":"))
			(cond
				((and (>= (length (elem-get 0 _)) 4) (eql "lisp" (slice 0 4 (elem-get 0 _)))))
				((and (>= (length (elem-get 1 _)) 5) (eql "lisp_" (slice 0 5 (elem-get 1 _)))))
				(:t (push keys k) (push vals v))))) k v)
	(each (lambda (_)
		(def (defq b (Button)) :text _ :border 0
			:flow_flags  (logior +flow_flag_align_vcenter +flow_flag_align_hleft))
		(. index :add_child (. b :connect +event_button))) keys)
	(def vdu :vdu_width
		(reduce max (map (lambda (_)
			(reduce max (map length (split _ (ascii-char 10))))) vals))))

(defun vdu-print (vdu buf s)
	(each (lambda (c)
		(cond
			((eql c (ascii-char 10))
				;line feed and truncate
				(if (> (length (push buf "")) (const vdu_height))
					(setq buf (slice (const (dec (neg vdu_height))) -1 buf))))
			(:t	;char
				(elem-set -2 buf (cat (elem-get -2 buf) c))))) s)
	(. vdu :load buf 0 0 (length (elem-get -2 buf)) (dec (length buf))) buf)

(ui-window *window* (:color +argb_black)
	(ui-title-bar _ "Help" (0xea19) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill :font *env_terminal_font*)
		(ui-scroll index_scroll +scroll_flag_vertical :nil
			(ui-flow index (:flow_flags (logior +flow_flag_down +flow_flag_fillw) :color +argb_white)))
		(ui-vdu vdu (:vdu_height vdu_height :ink_color +argb_cyan))))

(defun main ()
	(populate-help)
	(bind '(w h) (. index :pref_size))
	(. index :change 0 0 (def index_scroll :min_width w) h)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(while (cond
		((= (defq id (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id)) +event_close)
			:nil)
		((= id +event_button)
			(defq _ (find-rev (sym (get :text (. *window* :find_id (getf msg +ev_msg_action_source_id)))) keys))
			(when _
				(setq text_buf (vdu-print vdu text_buf (str
					"----------------------" (ascii-char 10)
					(elem-get _ keys) (ascii-char 10)
					"----------------------" (ascii-char 10)
					(elem-get _ vals)
					"----------------------" (ascii-char 10) (ascii-char 10))))))
		(:t (. *window* :event msg))))
	(gui-sub *window*))
