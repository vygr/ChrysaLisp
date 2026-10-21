(case :nil
	(0 (import "lib/debug/frames.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/consts/colors.inc")

;;;; ── constants & globals ────────────────────────────────────────────────────

(defq +config_version 3 +heartbeat_us (* 10 1000000) +presence_timeout_us (* 15 1000000)
	+max_display 100 +max_history 500 +max_channels 128)

(enums +event +event_user
	(enum connect disconnect send attach))

(defq +event_channel_select_0 100 +event_channel_select_last (+ 100 +max_channels -1)
	+event_channel_delete_0 300 +event_channel_delete_last (+ 300 +max_channels -1))

(enums +select 0
	(enum main tip heartbeat chat picker))

; *chats*    — (Fmap) sys_id_hex -> (Fmap) name -> (list raw_msg_strings)
; *presence* — (Fmap) "<sys_id_hex>,<name>" -> last_beat_us
; *channel_index* — ordered list of (sys_id_hex name) pairs for event mapping
(defq +config_file (cat (get :home *env*) "chat.tre") *config* :nil *chats* (Fmap)
	*presence* (Fmap) *unread* (Fmap) *channel_index* (list)
	*current_sys_id* :nil *current_name* :nil
	*connected* :nil *chat_entry* :nil select :nil *picker_mbox* :nil
	;a file sent is no bigger than this, and those that come are kept here
	+max_file (* 8 1024 1024) +image_types ''(".png" ".jpg" ".jpeg" ".cpm" ".cwb" ".tga" ".svg")
	*my_sys_id_hex* (hex-encode (system-id)))

(defun channel-key (sys_id_hex cname)
	(cat (if sys_id_hex sys_id_hex "global") ":" cname))

(defun get-unread (sys_id_hex cname)
	(or (. *unread* :find (channel-key sys_id_hex cname)) 0))

(defun clear-unread (sys_id_hex cname)
	(. *unread* :insert (channel-key sys_id_hex cname) 0))

(defun inc-unread (sys_id_hex cname)
	(defq k (channel-key sys_id_hex cname))
	(. *unread* :update k (# (if %0 (inc %0) 1))))

;;;; ── widget tree ────────────────────────────────────────────────────────────

(ui-window *window* (:paper +argb_grey15)
	(ui-title-bar *title* "Chat" (+sym_close +sym_max +sym_min) +event_close)
	; single toolbar: [Dial] [Hangup] then name textfield fills the rest
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_dial +sym_hangup) +event_connect))
		(. (ui-textfield *chat_name* (:flow_flags +flow_right_fill :hint_text "your name..." :clear_text ""))
			:connect +event_connect))
	; main body: left channel list + right history pane
	(ui-flow *main_split* (:flow_flags +flow_right_fill)
		; left panel: vertically scrollable channel list
		(ui-scroll *channel_scroll* +scroll_flag_vertical (:min_width 200 :min_height 420)
			(ui-flow *channel_flow* (:flow_flags +flow_down_fill :paper +argb_grey15)))
		; right panel: history + bottom-pinned input bar
		(ui-flow _ (:flow_flags +flow_up_fill)
			(ui-flow *input_bar* (:flow_flags +flow_right_fill :border 1
					:paper +argb_white :ink +argb_black :font :font_button)
				(ui-label _ (:text "Chat:" :font :font_bold))
				(ui-tool-bar *attach_toolbar* () (ui-buttons (+sym_open) +event_attach))
				(. (ui-textfield *chat_text* (:flow_flags +flow_right_fill :hint_text "Type markdown message..." :clear_text ""))
					:connect +event_send))
			(ui-scroll *chat_scroll* +scroll_flag_vertical (:min_width 480 :min_height 380)
				(ui-flow *chat_flow* (:flow_flags +flow_down_fill :paper +argb_grey15))))))

;;;; ── config ─────────────────────────────────────────────────────────────────

(defun config-default ()
	(scatter (Emap)
		:version +config_version :name (or (get :user *env*) "guest")
		:max_display +max_display :chats (Fmap)))

(defun config-load ()
	(setq *config* (with-read-lock +config_file
		(tree-load (file-stream +config_file))))
	(if (or (not *config*) (/= (. *config* :find :version) +config_version))
		(setq *config* (config-default)))
	(def *chat_name* :clear_text (. *config* :find :name))
	(setq *chats* (. *config* :find :chats)))

(defun config-save ()
	(. *chats* :each (lambda (sys_id_hex name_map)
		(. name_map :each (lambda (cname msgs)
			(when (> (length msgs) +max_history)
				(. name_map :insert cname
					(slice msgs (- (length msgs) +max_history) -1)))))))
	(scatter *config* :name (get :clear_text *chat_name*) :chats *chats*)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

;;;; ── chat data helpers ───────────────────────────────────────────────────────

(defun ensure-chat-bucket (sys_id_hex cname)
	; ensure *chats*[sys_id_hex][cname] exists, return the name->msgs Fmap
	(defq sys_map (. *chats* :find sys_id_hex))
	(ifn sys_map (. *chats* :insert sys_id_hex (setq sys_map (Fmap))))
	(unless (. sys_map :find cname)
		(. sys_map :insert cname (list)))
	sys_map)

(defun get-chat-history (sys_id_hex cname)
	(if (and (defq sys_map (. *chats* :find sys_id_hex))
			(defq msgs (. sys_map :find cname)))
		msgs (list)))

(defun push-chat-msg (sys_id_hex cname raw_msg)
	(defq sys_map (ensure-chat-bucket sys_id_hex cname)
		msgs (. sys_map :find cname))
	(push msgs raw_msg))

; Snapshot of all known *Chat peers: list of (name mbox_hex sys_id_hex) triples
(defun enquire-snapshot ()
	(reduce (lambda (acc entry)
		(defq parts (split entry ","))
		(when (= (length parts) 4)
			(bind '(& mbox_hex sys_id_hex cname) parts)
			(push acc (list cname mbox_hex sys_id_hex)))
		acc)
		(mail-enquire "*Chat") (list)))

(defun presence-key (sys_id_hex cname)
	(cat sys_id_hex "," cname))

(defun is-online? (sys_id_hex cname)
	(if (defq last_beat (. *presence* :find (presence-key sys_id_hex cname)))
		(< (- (pii-time) last_beat) +presence_timeout_us) :nil))

(defun bump-presence (sys_id_hex cname)
	; any inbound activity (message or heartbeat) counts as online
	(. *presence* :insert (presence-key sys_id_hex cname) (pii-time)))

;;;; ── message formatting ─────────────────────────────────────────────────────

(defun parse-chat-msg (raw_msg)
	(defq sender "System" body (trim (or raw_msg "") "\t\n\r "))
	(when (starts-with "<" body)
		(when (defq q2 (find ">" body))
			(setq sender (slice body 1 q2)
				body (trim (slice body (inc q2) -1) "\t\n\r "))))
	(list sender body))

(defun format-chat-lines (sender body)
	(cond
		((eql body "Has joined the chat !")
			(list (cat "**" sender "** *has joined the chat.*") "" "---"))
		((eql body "Has left the chat !")
			(list (cat "**" sender "** *has left the chat.*") "" "---"))
		(:t
			(defq lines (list (cat "**" sender "**") ""))
			(each (lambda (l) (push lines l)) (split body "\n"))
			(push lines "" "---")
			lines)))

(defun create-file-view (sender path page_w)
	;a file in the chat: a picture is shown, no wider than the page, the
	;words of a .md are, and of anything else where it was kept is said
	(defq name (path-name path)
		ext (path-ext name))
	;one that came is kept as chat_<time>_<name>, it is shown by its name
	(and (starts-with "chat_" name) (defq under (find "_" (slice name 5 -1)))
		(setq name (slice name (+ 6 under) -1)))
	(defq view (Flow))
	(def view :flow_flags +flow_down_fill :paper +argb_grey15)
	(def (defq md (Md)) :page_width page_w :zoom 1.0 :base_font_size 13)
	(. view :add_child md)
	(cond
		((and (find ext +image_types) (defq canvas (canvas-load path +load_flag_free)))
			(. md :populate_lines (list (cat "**" sender "** *sent* `" name "`") ""))
			(bind '(w h) (. canvas :pref_size))
			(defq most (max 64 (- page_w 24)))
			(when (> w most)
				(defq small (Canvas most (max 1 (/ (* h most) w)) 1))
				(. small :resize canvas)
				(setq canvas small))
			(def (defq frame (Flow)) :flow_flags +flow_right :paper +argb_grey15)
			(. frame :add_child canvas)
			(. view :add_child frame))
		((and (eql ext ".md") (defq text (load path)))
			(. md :populate_lines (cat (list (cat "**" sender "** *sent* `" name "`") "")
				(split text (ascii-char 10)) (list "" "---"))))
		(:t (. md :populate_lines (list (cat "**" sender "** *sent* `" name "`") ""
			(cat "Kept as `" path "`") "" "---"))))
	view)

(defun create-chat-md (sender body page_w)
	;a message, or a file that came: "@file " and where it was kept
	(if (starts-with "@file " body)
		(create-file-view sender (slice body 6 -1) page_w)
		(def (defq md (Md)) :page_width page_w :zoom 1.0 :base_font_size 13)
		(. md :populate_lines (format-chat-lines sender body))
		md))

;;;; ── layout helpers ─────────────────────────────────────────────────────────

(defun chat-scroll-page-w ()
	(bind '(sw &ignore) (. *chat_scroll* :get_size))
	(bind '(vsw &ignore) (. (get :vslider *chat_scroll*) :pref_size))
	(max 360 (- sw vsw)))

(defun relayout-chat ()
	(defq page_w (chat-scroll-page-w))
	(each (lambda (child)
		(when (Md? child)
			(def child :page_width page_w)
			(. child :render))) (. *chat_flow* :children))
	(bind '(w h) (. *chat_flow* :pref_size))
	(. *chat_flow* :change_dirty 0 0 (max w page_w) h :t)
	(. *chat_flow* :layout)
	(.-> *chat_scroll* :layout :dirty_all)
	(when (nempty? (. *chat_flow* :children))
		(. *chat_scroll* :visible (last (. *chat_flow* :children)))))

(defun relayout-channel ()
	(bind '(& h) (. *channel_flow* :pref_size))
	(. *channel_flow* :change 0 0 (get :min_width *chat_scroll*) h :t)
	(.-> *channel_scroll* :layout :dirty_all))

(defun refresh-layout ()
	(relayout-chat)
	(relayout-channel))

;;;; ── right panel: chat history ──────────────────────────────────────────────

(defun render-chat-history ()
	(each (# (. %0 :sub)) (. *chat_flow* :children))
	(defq page_w (chat-scroll-page-w))
	(if (and (not *current_sys_id*) (not *current_name*))
		; no channel selected — show welcome
		(progn
			(def (defq md (Md)) :page_width page_w :zoom 1.0 :base_font_size 13)
			(. *chat_flow* :add_child md)
			(. md :populate_lines
				(list "# ChrysaLisp Chat" ""
					"Select a channel on the left, or type your name and click **Dial** to join."
					"" "*Your messages appear here.*")))
		; render history for selected (sys_id, name)
		(defq msgs (get-chat-history *current_sys_id* *current_name*)
			entries (if (> (length msgs) +max_display)
				(slice msgs (- (length msgs) +max_display) -1) msgs))
		(each (lambda (raw)
			(bind '(sender body) (parse-chat-msg raw))
			(. *chat_flow* :add_child (create-chat-md sender body page_w))) entries))
	(bind '(w h) (. *chat_flow* :pref_size))
	(. *chat_flow* :change_dirty 0 0 (max w page_w) h :t)
	(. *chat_flow* :layout)
	(.-> *chat_scroll* :layout :dirty_all)
	(when (nempty? (. *chat_flow* :children))
		(. *chat_scroll* :visible (last (. *chat_flow* :children)))))

;;;; ── left panel: channel list ───────────────────────────────────────────────

(defun build-channel-index ()
	; "global" synthetic row always first, then all known (sys_id, name) pairs
	(setq *channel_index* (list (list :nil "global")))
	(. *chats* :each (lambda (sys_id_hex name_map)
		(and sys_id_hex (nql sys_id_hex "global")
			(. name_map :each (lambda (cname &ignore)
				(unless (eql cname "global")
					(push *channel_index* (list sys_id_hex cname)))))))))

(defun render-channel-list ()
	(build-channel-index)
	(each (# (. %0 :sub)) (. *channel_flow* :children))
	(defq my_name (get :clear_text *chat_name*))
	(each (lambda (entry)
			(bind '(sys_id_hex cname) entry)
			(defq idx (!)
				is_selected (and (eql sys_id_hex *current_sys_id*) (eql cname *current_name*))
				is_self (and (eql sys_id_hex *my_sys_id_hex*) (eql cname my_name))
				online (cond
					((not sys_id_hex) *connected*)
					(is_self *connected*)
					(:t (is-online? sys_id_hex cname)))
				dot_ch (if online "+" "-")
				short_id (if sys_id_hex (slice sys_id_hex 0 (min 8 (length sys_id_hex))) "all")
				unread (if is_selected 0 (get-unread sys_id_hex cname))
				badge (if (> unread 0) (cat " (" (str unread) ")") "")
				lbl_txt (if (eql cname "global")
					(cat dot_ch " global" badge)
					(cat dot_ch " " cname "@" short_id badge))
				row_flow (Flow) row_btn (Button))
			(def row_flow :flow_flags +flow_right_fill)
			(def row_btn :text lbl_txt
				:paper (cond
					(is_selected +argb_red)
					((> unread 0) +argb_yellow)
					(:t +argb_green))
				:ink (if online +argb_black +argb_grey8))
			(view-variant row_btn :font :font_button)
			(. row_btn :connect (+ +event_channel_select_0 idx))
			(defq del_btn (Button))
			(def del_btn :text "x" :ink +argb_black)
			(. del_btn :connect (+ +event_channel_delete_0 idx))
			(. row_flow :add_child del_btn)
			(. row_flow :add_child row_btn)
			(. *channel_flow* :add_child row_flow))
		*channel_index*)
	(relayout-channel))

;;;; ── channel selection / deletion ───────────────────────────────────────────

(defun select-channel (idx)
	(when (< -1 idx (length *channel_index*))
		(bind '(sys_id_hex cname) (elem-get *channel_index* idx))
		(setq *current_sys_id* sys_id_hex *current_name* cname)
		(clear-unread sys_id_hex cname)
		(render-channel-list)
		(render-chat-history)))

(defun delete-channel (idx)
	(when (< -1 idx (length *channel_index*))
		(bind '(sys_id_hex cname) (elem-get *channel_index* idx))
		(. *unread* :erase (channel-key sys_id_hex cname))
		(cond
			((or (not sys_id_hex) (eql cname "global"))
				; clear global chat history, preserve the channel
				(when (defq sys_map (. *chats* :find :nil))
					(. sys_map :insert "global" (list)))
				(when (defq sys_map (. *chats* :find "global"))
					(. sys_map :insert "global" (list))))
			(:t ; peer channel: delete from *chats*
				(when (defq sys_map (. *chats* :find sys_id_hex))
					(. sys_map :erase cname)
					(and (eql *current_sys_id* sys_id_hex) (eql *current_name* cname)
						(setq *current_sys_id* :nil *current_name* :nil)))))
		(config-save)
		(render-channel-list)
		(render-chat-history)))

;;;; ── incoming message routing ───────────────────────────────────────────────

(defun add-chat-message (raw_msg)
	; check for DM prefix "DM:<target>:<message>"
	(defq is_dm :nil target_recipient :nil actual_msg raw_msg)
	(when (starts-with "DM:" raw_msg)
		(defq rest_msg (slice raw_msg 3 -1)
			col_pos (find ":" rest_msg))
		(when col_pos
			(setq is_dm :t
				target_recipient (slice rest_msg 0 col_pos)
				actual_msg (slice rest_msg (inc col_pos) -1))))
	(bind '(sender body) (parse-chat-msg actual_msg))
	(defq sys_id_hex :nil)
	(each (lambda (triple)
		(bind '(cname & s_id) triple)
		(when (eql cname sender) (setq sys_id_hex s_id)))
		(enquire-snapshot))
	(ifn sys_id_hex (setq sys_id_hex "unknown"))
	(bump-presence sys_id_hex sender)
	(defq full_msg (cat "<" sender "> " body "\n"))
	(cond
		(is_dm
			; private direct message: store only in that peer's bucket
			(ensure-chat-bucket sys_id_hex sender)
			(push-chat-msg sys_id_hex sender full_msg)
			; append live if viewing this peer; otherwise increment unread
			(if (and (eql sys_id_hex *current_sys_id*) (eql sender *current_name*))
				(progn
					(defq page_w (chat-scroll-page-w))
					(while (>= (length (. *chat_flow* :children)) +max_display)
						(. (first (. *chat_flow* :children)) :sub))
					(defq md (create-chat-md sender body page_w))
					(. *chat_flow* :add_child md)
					(bind '(w h) (. *chat_flow* :pref_size))
					(. *chat_flow* :change_dirty 0 0 (max w page_w) h :t)
					(. *chat_flow* :layout)
					(.-> *chat_scroll* :layout :dirty_all)
					(. *chat_scroll* :visible md))
				(inc-unread sys_id_hex sender)))
		(:t
			; global broadcast: store only in global bucket
			(ensure-chat-bucket :nil "global")
			(push-chat-msg :nil "global" full_msg)
			; append live if viewing global; otherwise increment unread
			(if (or (not *current_name*) (eql *current_name* "global"))
				(progn
					(defq page_w (chat-scroll-page-w))
					(while (>= (length (. *chat_flow* :children)) +max_display)
						(. (first (. *chat_flow* :children)) :sub))
					(defq md (create-chat-md sender body page_w))
					(. *chat_flow* :add_child md)
					(bind '(w h) (. *chat_flow* :pref_size))
					(. *chat_flow* :change_dirty 0 0 (max w page_w) h :t)
					(. *chat_flow* :layout)
					(.-> *chat_scroll* :layout :dirty_all)
					(. *chat_scroll* :visible md))
				(inc-unread :nil "global"))))
	(config-save)
	(render-channel-list))

;;;; ── heartbeat ───────────────────────────────────────────────────────────────

(defun send-heartbeat ()
	(defq my_name (get :clear_text *chat_name*))
	(bump-presence *my_sys_id_hex* my_name)
	(defq hb (cat "HEARTBEAT:" *my_sys_id_hex* "," my_name)
		my_mbox_hex (if *connected* (hex-encode (elem-get select +select_chat)) ""))
	(each (lambda (triple)
		(bind '(& mbox_hex &ignore) triple)
		(unless (eql mbox_hex my_mbox_hex)
			(mail-send (hex-decode mbox_hex) hb)))
		(enquire-snapshot)))

(defun process-heartbeat (msg)
	; msg = "HEARTBEAT:<sys_id_hex>,<name>"
	(defq payload (slice msg (const (length "HEARTBEAT:")) -1)
		parts (split payload ","))
	(when (= (length parts) 2)
		(bind '(sys_id_hex cname) parts)
		(bump-presence sys_id_hex cname)
		(ensure-chat-bucket sys_id_hex cname)
		(render-channel-list)))

;;;; ── broadcast ───────────────────────────────────────────────────────────────

(defun broadcast (text &optional is_dm target_cname target_sys_id)
	(defq snap (enquire-snapshot)
		my_mbox_hex (if *connected* (hex-encode (elem-get select +select_chat)) ""))
	(if is_dm
		; direct message: send only to the matched peer, never self
		(each (lambda (triple)
			(bind '(cname mbox_hex sys_id_hex) triple)
			(and (eql cname target_cname)
					(eql sys_id_hex target_sys_id)
					(nql mbox_hex my_mbox_hex)
				(mail-send (hex-decode mbox_hex) text))) snap)
		; global broadcast: send to all nodes except self
		(each (lambda (triple)
			(bind '(& mbox_hex &ignore) triple)
			(unless (eql mbox_hex my_mbox_hex)
				(mail-send (hex-decode mbox_hex) text))) snap)))

;;;; ── connect / disconnect ────────────────────────────────────────────────────

(defun connect-chat ()
	(defq my_name (trim (get :clear_text *chat_name*)))
	(when (eql my_name "") (setq my_name (or (get :user *env*) "guest")))
	(def *chat_name* :clear_text my_name)
	; if name changed while connected, forget old service entry
	(when (and *connected* *chat_entry*)
		(mail-forget *chat_entry*)
		(setq *chat_entry* :nil))
	(setq *chat_entry* (mail-declare (elem-get select +select_chat) "*Chat" my_name)
		*connected* :t)
	; register presence and channel bucket immediately
	(bump-presence *my_sys_id_hex* my_name)
	(ensure-chat-bucket *my_sys_id_hex* my_name)
	(each (lambda ((cname & s_id))
		(ensure-chat-bucket s_id cname))
		(enquire-snapshot))
	(send-heartbeat)
	; record and render join announcement locally in global
	(defq join_msg (cat "<" my_name "> Has joined the chat !\n"))
	(ensure-chat-bucket :nil "global")
	(push-chat-msg :nil "global" join_msg)
	(when (or (not *current_name*) (eql *current_name* "global"))
		(defq page_w (chat-scroll-page-w)
			md (create-chat-md my_name "Has joined the chat !" page_w))
		(while (>= (length (. *chat_flow* :children)) +max_display)
			(. (first (. *chat_flow* :children)) :sub))
		(. *chat_flow* :add_child md)
		(bind '(w h) (. *chat_flow* :pref_size))
		(. *chat_flow* :change_dirty 0 0 (max w page_w) h :t)
		(. *chat_flow* :layout)
		(.-> *chat_scroll* :layout :dirty_all)
		(. *chat_scroll* :visible md))
	(broadcast join_msg)
	(config-save)
	(render-channel-list))

(defun disconnect-chat ()
	(when *connected*
		(defq my_name (get :clear_text *chat_name*)
			leave_msg (cat "<" my_name "> Has left the chat !\n"))
		; record and render leave announcement locally in global
		(ensure-chat-bucket :nil "global")
		(push-chat-msg :nil "global" leave_msg)
		(when (or (not *current_name*) (eql *current_name* "global"))
			(defq page_w (chat-scroll-page-w)
				md (create-chat-md my_name "Has left the chat !" page_w))
			(while (>= (length (. *chat_flow* :children)) +max_display)
				(. (first (. *chat_flow* :children)) :sub))
			(. *chat_flow* :add_child md)
			(bind '(w h) (. *chat_flow* :pref_size))
			(. *chat_flow* :change_dirty 0 0 (max w page_w) h :t)
			(. *chat_flow* :layout)
			(.-> *chat_scroll* :layout :dirty_all)
			(. *chat_scroll* :visible md))
		(broadcast leave_msg)
		(mail-forget *chat_entry*)
		(. *presence* :erase (presence-key *my_sys_id_hex* my_name))
		(setq *chat_entry* :nil *connected* :nil)
		(config-save)
		(render-channel-list)))

;;;; ── files ───────────────────────────────────────────────────────────────────

;A file goes as one message: "FILE:", who it is for if it is to one,
;and then a line each, who sent it and its name, and the bytes of it.
;What comes is kept in the user's folder, and the chat has where

(defun action-attach ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mbox* (open-child "apps/system/files/child.lisp" +kn_call_pin))
		(cat (elem-get select +select_picker) "Send a file..." "|" ".")))

(defun file-keep (name data)
	; (file-keep name data) -> path
	(defq path (cat (get :home *env*) "chat_" (str (pii-time)) "_" name))
	(save data path)
	path)

(defun send-file (path)
	;to the channel that is shown, as a message is sent
	(and (> (length path) 0) (defq data (load path))
		(cond
			((> (length data) +max_file)
				(add-chat-message (cat "<System> " path " is more than " (str (/ +max_file 1048576)) "MB, not sent")))
			(:t (unless *connected*
					(connect-chat)
					(mail-timeout (elem-get select +select_heartbeat) +heartbeat_us 0))
				(defq my_name (get :clear_text *chat_name*)
					name (path-name path)
					is_dm (and *current_sys_id* (nql *current_name* "global"))
					msg (cat "FILE:" (if is_dm *current_name* "") (ascii-char 10) my_name (ascii-char 10)
						name (ascii-char 10) data)
					entry (cat "<" my_name "> @file " path "\n"))
				(cond
					(is_dm
						(ensure-chat-bucket *current_sys_id* *current_name*)
						(push-chat-msg *current_sys_id* *current_name* entry)
						(broadcast msg :t *current_name* *current_sys_id*))
					(:t (ensure-chat-bucket :nil "global")
						(push-chat-msg :nil "global" entry)
						(broadcast msg :nil)))
				(config-save)
				;shown here as it is sent, as a message is
				(defq page_w (chat-scroll-page-w) view (create-chat-md my_name (trim (slice entry (inc (find ">" entry)) -1) (const (char-class " \t\r\n"))) page_w))
				(while (>= (length (. *chat_flow* :children)) +max_display)
					(. (first (. *chat_flow* :children)) :sub))
				(. *chat_flow* :add_child view)
				(bind '(w h) (. *chat_flow* :pref_size))
				(. *chat_flow* :change_dirty 0 0 (max w page_w) h :t)
				(. *chat_flow* :layout)
				(.-> *chat_scroll* :layout :dirty_all)
				(. *chat_scroll* :visible view)))))

(defun parse-file-msg (msg)
	; (parse-file-msg msg) -> :nil | (target sender name data)
	;the three lines after FILE: and then the bytes, which may have any
	(defq nl (ascii-char 10) a (find nl msg))
	(when a
		(defq rest1 (slice msg (inc a) -1) b (find nl rest1))
		(when b
			(defq rest2 (slice rest1 (inc b) -1) c (find nl rest2))
			(when c
				(list (slice msg 5 a) (slice rest1 0 b)
					(slice rest2 0 c) (slice rest2 (inc c) -1))))))

(defun receive-file (msg)
	;a file that came, kept, and in the chat as a message is. A name is
	;only a name, no folder of the sender's is made here
	(when (defq parts (parse-file-msg msg))
		(bind '(target sender name data) parts)
		(defq path (file-keep (path-name name) data))
		(add-chat-message (cat (if (nempty? target) (cat "DM:" target ":") "")
			"<" sender "> @file " path))))

;;;; ── tooltips ────────────────────────────────────────────────────────────────

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar* '("dial / join" "hangup / leave"))
	(ui-tool-tips *attach_toolbar* '("attach")))

;;;; ── main ───────────────────────────────────────────────────────────────────

(defun main ()
	(setq select (task-mboxes +select_size))
	(tooltips)
	(config-load)
	(render-channel-list)
	(render-chat-history)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(defq running :t)
	(while running
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_tip
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			(+select_heartbeat
				(mail-timeout (elem-get select +select_heartbeat) +heartbeat_us 0)
				(when *connected* (send-heartbeat))
				(render-channel-list))
			(+select_chat
				(cond
					((starts-with "HEARTBEAT:" *msg*) (process-heartbeat *msg*))
					((starts-with "FILE:" *msg*) (receive-file *msg*))
					((add-chat-message *msg*))))
			(+select_picker
				;the file picker says what file, or "" for none
				(defq path (trim *msg*))
				(mail-send *picker_mbox* "")
				(setq *picker_mbox* :nil)
				(send-file path))
			(+select_main
				(defq id (getf *msg* +ev_msg_target_id))
				(cond
					((= (getf *msg* +ev_msg_type) +ev_type_action)
						(cond
							((= id +event_close)
								(setq running :nil))
							((= id +event_connect)
								(connect-chat)
								(mail-timeout (elem-get select +select_heartbeat) +heartbeat_us 0))
							((= id +event_disconnect)
								(disconnect-chat))
							((= id +event_attach)
								(action-attach))
							((= id +event_send)
								(defq msg_text (trim (get :clear_text *chat_text*)))
								(when (nempty? msg_text)
									(unless *connected*
										(connect-chat)
										(mail-timeout (elem-get select +select_heartbeat) +heartbeat_us 0))
									(defq my_name (get :clear_text *chat_name*)
										full_msg (cat "<" my_name "> " msg_text "\n")
										is_dm (and *current_sys_id* (nql *current_name* "global")))
									(cond
										(is_dm
											; record in this peer channel's conversation
											(ensure-chat-bucket *current_sys_id* *current_name*)
											(push-chat-msg *current_sys_id* *current_name* full_msg)
											; send direct message to the selected peer only
											(broadcast (cat "DM:" *current_name* ":" full_msg) :t *current_name* *current_sys_id*))
										(:t
											; record in global channel only
											(ensure-chat-bucket :nil "global")
											(push-chat-msg :nil "global" full_msg)
											; broadcast to all peers
											(broadcast full_msg :nil)))
									; append live to the current viewing area
									(defq page_w (chat-scroll-page-w)
										md (create-chat-md my_name msg_text page_w))
									(while (>= (length (. *chat_flow* :children)) +max_display)
										(. (first (. *chat_flow* :children)) :sub))
									(. *chat_flow* :add_child md)
									(bind '(w h) (. *chat_flow* :pref_size))
									(. *chat_flow* :change_dirty 0 0 (max w page_w) h :t)
									(. *chat_flow* :layout)
									(.-> *chat_scroll* :layout :dirty_all)
									(. *chat_scroll* :visible md)
									(config-save)
									(set *chat_text* :clear_text "" :cursor 0 :anchor 0)
									(.-> *chat_text* :layout :dirty)))
							((<= +event_channel_select_0 id +event_channel_select_last)
								(select-channel (- id +event_channel_select_0)))
							((<= +event_channel_delete_0 id +event_channel_delete_last)
								(delete-channel (- id +event_channel_delete_0)))
							((= id +event_min)
								(bind '(x y w h)
									(apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
								(. *window* :change_dirty x y w h)
								(refresh-layout))
							((= id +event_max)
								(bind '(x y) (. *window* :get_pos))
								(bind '(mx my mw mh) (gui-info))
								(defq target_w (min 1100 (- mw 40)) target_h (min 760 (- mh 40)))
								(bind '(x y w h) (view-fit x y target_w target_h))
								(. *window* :change_dirty x y w h)
								(refresh-layout))))
					(:t (. *window* :event *msg*))))))
	(when *connected*
		(defq my_name (get :clear_text *chat_name*))
		(broadcast (cat "<" my_name "> Has left the chat !"))
		(mail-forget *chat_entry*))
	(config-save)
	(gui-sub-rpc *window*))