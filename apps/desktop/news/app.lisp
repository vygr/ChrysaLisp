(case :nil
	(0 (import "lib/debug/frames.inc"))
	(1 (import "lib/debug/profile.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/consts/colors.inc")
(import "lib/text/searching.inc")

(enums +select 0
	(enum main tip timer worker trash))

(enums +event 0
	(enum close max min)
	(enum refresh category))

(defq +event_story_0 100 +max_stories 50
	*config* :nil *config_version* 1
	*config_file* (cat *env_home* "news.tre") *selected_category* "top" *selected_id* 0
	*current_stories* (list) *selected_story* :nil *comments_cache* (Fmap 31)
	*post_content_cache* (Fmap 31) *cat_bar* :nil *btn_refresh* :nil *status_label* :nil
	*story_scroll* :nil *story_container* :nil *detail_scroll* :nil *detail_container* :nil
	*story_count_label* :nil *item_info_label* :nil)

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:selected_category "top"
		:selected_id 0))

(defun config-load ()
	(lock-claim-rpc *config_file*)
	(defq old_config :nil)
	(if (defq stream (file-stream *config_file*))
		(setq old_config (tree-load stream)))
	(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
		(setq *config* (config-default))
		(setq *config* old_config))
	(setq *selected_category* (. *config* :find :selected_category)
		*selected_id* (. *config* :find :selected_id))
	(lock-release-rpc *config_file*))

(defun config-save ()
	(lock-claim-rpc *config_file*)
	(ifn *config* (setq *config* (Emap)))
	(scatter *config*
		:version *config_version*
		:selected_category *selected_category*
		:selected_id *selected_id*)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*))
	(lock-release-rpc *config_file*))

(defun clean-hn-text (text)
	(ifn (str? text) ""
		(replace-regex
			(reduce! (const replace-str)
				'(("<p>" "</p>" "<pre><code>" "</code></pre>" "<code>" "</code>" "<i>" "</i>" "<b>" "</b>"
					"&#x27;" "&#39;" "&#x2F;" "&quot;" "&lt;" "&gt;" "&amp;")
				("\n\n" "" "\n```\n" "\n```\n" "`" "`" "*" "*" "**" "**"
					"'" "'" "/" "\q" "<" ">" "&")) text)
			"<[^>]+>" "")))

(defun truncate-title (title max_len)
	(if (<= (length title) max_len)
		title
		(cat (slice title 0 (- max_len 3)) "...")))

(defun category-to-idx (cat)
	(case cat
		("top" 0)
		("newest" 1)
		("show" 2)
		("ask" 3)
		("jobs" 4)
		(:t 0)))

(defun idx-to-category (idx)
	(case idx
		(0 "top")
		(1 "newest")
		(2 "show")
		(3 "ask")
		(4 "jobs")
		(:t "top")))

(defun format-story-markdown (story post_content comments)
	(bind '(id title points user time_ago comments_count url domain) story)
	(defq lines (list (cat "# " title) ""
		(cat "**" (str points) " points** by *" user "* " time_ago " | **" (str comments_count) " comments** | `" (if (nempty? domain) domain "hacker-news") "`") ""))
	(when (and url (not (starts-with "item?id=" url)))
		(push lines (cat "**Article Link**: `" url "`") ""))
	(when (and post_content (nempty? (trim post_content)))
		(push lines "---" "" (clean-hn-text post_content) ""))
	(push lines "---" "")
	(if (not comments)
		(push lines "*Fetching discussion from Hacker News...*")
		(if (empty? comments)
			(push lines "*No comments posted yet.*")
			(progn
				(defq max_c (min (length comments) 30))
				(push lines (cat "### Comments (" (str (length comments)) ")") "")
				(each (lambda (c)
					(bind '(c_user c_time c_body) c)
					(push lines (cat "* **" c_user "** *" c_time "*:"))
					(defq cleaned_body (clean-hn-text c_body))
					(each (lambda (cline)
						(if (nempty? (trim cline))
							(push lines (cat "  " cline))))
						(split cleaned_body "\n"))
					(push lines ""))
					(slice comments 0 max_c)))))
	lines)

(defun render-detail-pane (story post_content comments)
	(each (# (. %0 :sub)) (. *detail_container* :children))
	(bind '(sw &ignore) (. *detail_scroll* :get_size))
	(bind '(vsw &ignore) (if (defq vs (get :vslider *detail_scroll*))
			(. vs :get_constraint) '(16 0)))
	(defq page_w (max 420 (- sw vsw 16))
		lines (format-story-markdown story post_content comments))
	(def (defq md (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
	(. *detail_container* :add_child md)
	(. md :populate_lines lines)
	(bind '(w h) (. *detail_container* :pref_size))
	(. *detail_container* :change_dirty 0 0 (max w page_w) h :t)
	(.-> *detail_scroll* :layout :dirty_all))

(defun trigger-fetch-item (item_id worker_mbox trash_mbox)
	(def (. *status_label* :dirty) :text (cat "Loading #" (str item_id) "..."))
	(. *status_label* :layout)
	(defq url (cat "http://node-hnapi.herokuapp.com/item/" (str item_id))
		task_code (str `(progn
			(import "service/net/app.inc")
			(import "lib/net/http.inc")
			(import "lib/net/json.inc")
			(ensure-net-service)
			(defq result :nil)
			(catch
				(progn
					(when (defq resp (http-get ,url))
						(defq body (http-body-str resp))
						(when (and body (starts-with "{" (trim body)))
							(when (defq json (json-parse body))
								(defq raw_comments (or (pfind json :comments) (list))
									comment_list (list)
									post_content (or (pfind json :content) ""))
								(each (lambda (c)
									(push comment_list (list (or (pfind c :user) "anon")
										(or (pfind c :time_ago) "") (or (pfind c :content) ""))))
									raw_comments)
								(setq result (list :item ,item_id post_content comment_list))))))
				(progn (setq result :nil) :t))
			(mail-send (hex-decode ,(hex-encode worker_mbox)) (str result)))))
	(open-task task_code (slice (task-mbox) +long_size -1) +kn_call_run 0 trash_mbox))

(defun render-loading-stories ()
	(each (# (. %0 :sub)) (. *story_container* :children))
	(def (defq lbl (Label)) :text "Fetching stories..." :font *env_button_font* :ink_color +argb_grey8)
	(. *story_container* :add_child lbl)
	(bind '(& h) (. *story_container* :pref_size))
	(bind '(sw &ignore) (. *story_scroll* :get_size))
	(bind '(vsw &ignore) (if (defq vs (get :vslider *story_scroll*)) (. vs :get_constraint) '(16 0)))
	(defq cw (max 200 (- sw vsw)))
	(. *story_container* :change_dirty 0 0 cw (max h 40) :t)
	(.-> *story_scroll* :layout :dirty_all))

(defun render-story-list ()
	(each (# (. %0 :sub)) (. *story_container* :children))
	(defq sel_id (if *selected_story* (first *selected_story*) *selected_id*))
	(each (lambda (story)
		(bind '(id title points user time_ago comments_count url domain) story)
		(defq idx (!) is_selected (= id sel_id)
			card_flow (Flow) title_btn (Button) meta_flow (Flow)
			score_lbl (Label) meta_lbl (Label))
		(def card_flow :flow_flags +flow_down_fill :border 1)
		(def title_btn :text (cat (str (inc idx)) ". " (truncate-title title 24))
			:font *env_button_font* :border (if is_selected 1 0))
		(when is_selected
			(def card_flow :color (canvas-brighter (get :color *window*)))
			(def title_btn :color 0xffff6600))
		(. title_btn :connect (+ +event_story_0 idx))
		(def meta_flow :flow_flags +flow_right_fill)
		(def score_lbl :text (cat (str points) " pts") :ink_color 0xffff6600
			:font *env_small_font* :border 0)
		(def meta_lbl :text (cat " * " user " (" (str comments_count) ")")
			:ink_color +argb_grey8 :font *env_small_font* :border 0)
		(.-> meta_flow (:add_child score_lbl) (:add_child meta_lbl))
		(.-> card_flow (:add_child title_btn) (:add_child meta_flow))
		(. *story_container* :add_child card_flow))
		*current_stories*)
	(def (. *story_count_label* :dirty) :text (cat (str (length *current_stories*)) " stories"))
	(. *story_count_label* :layout)
	(bind '(& h) (. *story_container* :pref_size))
	(bind '(sw &ignore) (. *story_scroll* :get_size))
	(bind '(vsw &ignore) (if (defq vs (get :vslider *story_scroll*))
			(. vs :get_constraint) '(16 0)))
	(defq cw (max 200 (- sw vsw)))
	(. *story_container* :change_dirty 0 0 cw h :t)
	(.-> *story_scroll* :layout :dirty_all))

(defun select-story (idx worker_mbox trash_mbox)
	(when (< -1 idx (length *current_stories*))
		(setq *selected_story* (elem-get *current_stories* idx))
		(bind '(id title points user time_ago comments_count url domain)
			*selected_story*)
		(setq *selected_id* id)
		(config-save)
		(def (. *item_info_label* :dirty) :text (cat "#" (str id) " | " domain))
		(. *item_info_label* :layout)
		(render-story-list)
		(if (defq cached_comments (. *comments_cache* :find id))
			(render-detail-pane *selected_story* (. *post_content_cache* :find id) cached_comments)
			(render-detail-pane *selected_story* :nil :nil)
			(trigger-fetch-item id worker_mbox trash_mbox))))

(defun trigger-fetch-feed (category worker_mbox trash_mbox)
	(def (. *status_label* :dirty) :text (cat "Updating " category "..."))
	(defq endpoint (case category ("top" "news") ("newest" "newest")
			("show" "show") ("ask" "ask") ("jobs" "jobs") (:t "news"))
		url (cat "http://node-hnapi.herokuapp.com/" endpoint)
		task_code (str `(progn
			(import "service/net/app.inc")
			(import "lib/net/http.inc")
			(import "lib/net/json.inc")
			(ensure-net-service)
			(defq result :nil)
			(catch
				(progn
					(when (defq resp (http-get ,url))
						(defq body (http-body-str resp))
						(when (and body (starts-with "[" (trim body)))
							(when (defq json (json-parse body))
								(defq story_list (list))
								(each (lambda (item)
									(defq id (or (pfind item :id) 0)
										title (or (pfind item :title) "Untitled")
										points (or (pfind item :points) 0)
										user (or (pfind item :user) "anon")
										time_ago (or (pfind item :time_ago) "")
										comments_count (or (pfind item :comments_count) 0)
										url_link (or (pfind item :url) "")
										domain (or (pfind item :domain) ""))
									(push story_list (list id title points user time_ago
										comments_count url_link domain)))
									json)
								(setq result (list :feed ,category story_list))))))
				(progn (setq result :nil) :t))
			(mail-send (hex-decode ,(hex-encode worker_mbox)) (str result)))))
	(open-task task_code (slice (task-mbox) +long_size -1) +kn_call_run 0 trash_mbox))

(defun select-category (cat_name worker_mbox trash_mbox)
	(setq *selected_category* cat_name)
	(config-save)
	(when *cat_bar*
		(. *cat_bar* :set_selected (category-to-idx cat_name)))
	(render-loading-stories)
	(trigger-fetch-feed cat_name worker_mbox trash_mbox))

(ui-window *window* (:color +argb_grey15)
	(ui-title-bar _ "Hacker News" (0xea19 0xea1b 0xea1a) +event_close)
	; Header Navigation Bar
	(ui-flow header_bar (:flow_flags +flow_right_fill :border 1 :font *env_button_font*)
		(ui-label _ (:text " HN " :color 0xffff6600 :ink_color +argb_white :font *env_bold_font* :border 1))
		(. (ui-radio-bar *cat_bar* ("Top" "Newest" "Show HN" "Ask HN" "Jobs") (:font *env_button_font*)) :connect +event_category)
		(. (ui-button *btn_refresh* (:text "Refresh" :color 0xffff6600 :ink_color +argb_white)) :connect +event_refresh)
		(ui-label *status_label* (:text "Connecting..." :border 0 :ink_color +argb_grey8)))
	; makes the main_split use up all the remaining space !
	(ui-flow _ (:flow_flags +flow_up_fill)
		; Footer Status Bar
		(ui-flow status_bar (:flow_flags +flow_right_fill :border 1)
			(ui-label _ (:min_width 4 :border 0))
			(ui-label *story_count_label* (:text "0 stories" :font *env_small_font* :border 0 :min_width 80))
			(ui-label *item_info_label* (:text "Select a story" :font *env_small_font* :border 0 :ink_color +argb_grey8 :min_width 160)))
		; Split Pane Body: Story List (Left) + Detail/Md Viewer (Right)
		(ui-flow main_split (:flow_flags +flow_right_fill)
			(ui-scroll *story_scroll* +scroll_flag_vertical (:min_width 220 :min_height 460)
				(ui-flow *story_container* (:flow_flags +flow_down_fill :color +argb_grey15)))
			(ui-scroll *detail_scroll* +scroll_flag_both (:min_width 500 :min_height 460)
				(ui-flow *detail_container* (:flow_flags +flow_down_fill :color +argb_grey15))))))

(defun main ()
	(config-load)
	(defq select (task-mboxes +select_size) *running* :t
		refresh_interval (* 300 1000000)) ; 5 minutes
	(def *window* :tip_mbox (elem-get select +select_tip))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (.-> *window* (:change x y w h :t) :dirty_all))
	(. *cat_bar* :set_selected (category-to-idx *selected_category*))
	(render-loading-stories)
	(trigger-fetch-feed *selected_category* (elem-get select +select_worker) (elem-get select +select_trash))
	(mail-timeout (elem-get select +select_timer) refresh_interval 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_worker
				(if (and *msg* (defq res (first (read (string-stream *msg*)))) (list?? res))
					(case (first res)
						(:feed
							(bind '(& cat_name story_list) res)
							(setq *current_stories* story_list)
							(def (. *status_label* :dirty) :text "Updated feed")
							(if (and *current_stories* (nempty? *current_stories*))
								(select-story 0 (elem-get select +select_worker) (elem-get select +select_trash))
								(render-story-list)))
						(:item
							(bind '(& item_id post_content comments) res)
							(. *comments_cache* :insert item_id comments)
							(. *post_content_cache* :insert item_id post_content)
							(def (. *status_label* :dirty) :text "Loaded discussion")
							(when (and *selected_story* (= (first *selected_story*) item_id))
								(render-detail-pane *selected_story* post_content comments))))
					(def (. *status_label* :dirty) :text "Fetch error")
					(.-> *status_label* :layout :dirty)))
			(+select_timer
				(mail-timeout (elem-get select +select_timer) refresh_interval 0)
				(trigger-fetch-feed *selected_category* (elem-get select +select_worker) (elem-get select +select_trash)))
			(+select_tip
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			(+select_main
				(defq id (getf *msg* +ev_msg_target_id))
				(cond
					((= id +event_close)
						(setq *running* :nil))
					((= id +event_min)
						(bind '(x y w h)
							(apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
						(. *window* :change_dirty x y w h)
						(when *selected_story*
							(render-detail-pane *selected_story* (. *post_content_cache* :find *selected_id*) (. *comments_cache* :find *selected_id*))))
					((= id +event_max)
						(bind '(x y) (. *window* :get_pos))
						(bind '(mx my mw mh) (gui-info))
						(defq target_w (min 1200 (- mw 40))
							target_h (min 800 (- mh 40)))
						(bind '(x y w h) (view-fit x y target_w target_h))
						(. *window* :change_dirty x y w h)
						(when *selected_story*
							(render-detail-pane *selected_story* (. *post_content_cache* :find *selected_id*) (. *comments_cache* :find *selected_id*))))
					((= id +event_refresh)
						(trigger-fetch-feed *selected_category* (elem-get select +select_worker) (elem-get select +select_trash)))
					((= id +event_category)
						(defq c_idx (. *cat_bar* :get_selected))
						(when c_idx
							(select-category (idx-to-category c_idx) (elem-get select +select_worker) (elem-get select +select_trash))))
					((<= +event_story_0 id (const (+ +event_story_0 +max_stories -1)))
						(select-story (- id +event_story_0) (elem-get select +select_worker) (elem-get select +select_trash)))
					((. *window* :event *msg*))))))
	(config-save)
	(gui-sub-rpc *window*))
