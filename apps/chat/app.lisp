(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close)
	(enum connect disconnect)
	(enum send))

(enums +select 0
	(enum main tip chat))

(defq vdu_width 60 vdu_height 30)

(ui-window *window* (:color +argb_grey4)
	(ui-title-bar _ "Chat" (0xea19) +event_close)
	(ui-tool-bar main_toolbar ()
		(ui-buttons (0xe9ed 0xe9e8) +event_connect))
	(ui-flow _ (:flow_flags +flow_right_fill :ink_color +argb_white :color +argb_white :ink_color +argb_black)
		(ui-label _ (:text "User:"))
		(. (ui-textfield chat_user (:clear_text "" :hint_text "username")) :connect +event_connect))
	(ui-vdu vdu (:vdu_width vdu_width :vdu_height vdu_height :ink_color +argb_white))
	(ui-flow _ (:flow_flags +flow_right_fill :ink_color +argb_white :color +argb_white :ink_color +argb_black)
		(ui-label _ (:text "Chat:"))
		(. (ui-textfield chat_text (:clear_text "" :hint_text "type here !")) :connect +event_send)))

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

(defun broadcast (text)
	(setq text (cat "<" (get :clear_text chat_user) "> " text (ascii-char 10)))
	(each (# (mail-send (to-net-id (elem-get 1 (split %0 ","))) text)) (mail-enquire "CHAT_SERVICE")))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("join" "leave")))

(defun main ()
	(defq id :t text_buf (list "") entry :nil select (alloc-select (dec +select_size)))
	(tooltips)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(while id
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_chat)
				;chat text from network
				(setq text_buf (vdu-print vdu text_buf *msg*)))
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				;close
				(setq id :nil))
			((= id +event_connect)
				;connect to network ?
				(unless (or entry (eql "" (get :clear_text chat_user)))
					(push select (mail-alloc-mbox))
					(setq entry (mail-declare (elem-get +select_chat select) "CHAT_SERVICE" "Chat Service 0.1"))
					(broadcast "Has joined the chat !")))
			((= id +event_disconnect)
				;disconnect to network
				(when entry
					(broadcast "Has left the chat !")
					(mail-free-mbox (pop select))
					(mail-forget entry)
					(setq entry :nil)))
			((= id +event_send)
				;send to network
				(broadcast (get :clear_text chat_text))
				(set chat_text :clear_text "" :cursor 0 :anchor 0)
				(.-> chat_text :layout :dirty))
			(:t (. *window* :event *msg*))))
	(when entry
		(broadcast "Has left the chat !")
		(mail-forget entry))
	(free-select select)
	(gui-sub *window*))
