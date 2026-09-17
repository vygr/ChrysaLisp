(case :nil
	(0 (import "lib/debug/frames.inc"))
	(1 (import "lib/debug/profile.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/colors.inc")
(import "lib/text/searching.inc")

(enums +select 0
	(enum main tip timer worker))

(enums +event 0
	(enum close refresh
		tab_top tab_new tab_show tab_ask tab_jobs
		story_0))

(defq
	+font_title (create-font "fonts/OpenSans-Bold.ctf" 14)
	+font_btn (create-font "fonts/OpenSans-Regular.ctf" 13)
	+font_bold (create-font "fonts/OpenSans-Bold.ctf" 13)
	+font_small (create-font "fonts/OpenSans-Regular.ctf" 11)
	+font_mono (create-font "fonts/Hack-Regular.ctf" 11)
	*config* :nil *config_version* 1 *config_file* (cat *env_home* "news.tre")
	*selected_category* "top" *selected_id* 0 *current_stories* (list)
	*selected_story* :nil *comments_cache* (Fmap 31) *post_content_cache* (Fmap 31)
	*btn_top* :nil *btn_new* :nil *btn_show* :nil *btn_ask* :nil *btn_jobs* :nil
	*btn_refresh* :nil *status_label* :nil *story_scroll* :nil
	*story_container* :nil *detail_scroll* :nil *detail_container* :nil
	*story_count_label* :nil *item_info_label* :nil)

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:selected_category "top"
		:selected_id 0))

(defun config-load ()
	(defq old_config :nil)
	(if (defq stream (file-stream *config_file*))
		(setq old_config (tree-load stream)))
	(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
		(setq *config* (config-default))
		(setq *config* old_config))
	(setq *selected_category* (. *config* :find :selected_category))
	(if (not (str? *selected_category*))
		(setq *selected_category* "top"))
	(setq *selected_id* (. *config* :find :selected_id))
	(if (not (num? *selected_id*))
		(setq *selected_id* 0)))

(defun config-save ()
	(if (not *config*)
		(setq *config* (Emap)))
	(scatter *config*
		:version *config_version*
		:selected_category *selected_category*
		:selected_id *selected_id*)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

(defun clean-hn-text (text)
	(if (not (str? text))
		""
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

(defun update-tab-buttons ()
	(defq tabs (list
		(list *btn_top* "top")
		(list *btn_new* "newest")
		(list *btn_show* "show")
		(list *btn_ask* "ask")
		(list *btn_jobs* "jobs")))
	(each (lambda ((btn cat_name))
		(when btn
			(if (eql cat_name *selected_category*)
				(def (. btn :dirty) :color 0xffff6600 :ink_color +argb_white)
				(progn
					(undef (. btn :dirty) :color)
					(undef (. btn :dirty) :ink_color)))))
		tabs))

(defun format-story-markdown (story post_content comments)
	(bind '(id title points user time_ago comments_count url domain) story)
	(defq lines (list
		(cat "# " title)
		""
		(cat "**" (str points) " points** by *" user "* " time_ago " | **" (str comments_count) " comments** | `" (if (nempty? domain) domain "hacker-news") "`")
		""))
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
	(bind '(sw &) (. *detail_scroll* :get_size))
	(bind '(vsw &) (if (get :vslider *detail_scroll*) (. (get :vslider *detail_scroll*) :get_constraint) '(16 0)))
	(defq page_w (max 420 (- sw vsw 16)))
	(def (defq md (Md))
		:page_width page_w
		:zoom 1.0
		:base_font_size 14)
	(. *detail_container* :add_child md)
	(defq lines (format-story-markdown story post_content comments))
	(. md :populate_lines lines)
	(bind '(w h) (. *detail_container* :pref_size))
	(. *detail_container* :change_dirty 0 0 (max w page_w) h :t)
	(.-> *detail_scroll* :layout :dirty_all))

(defun render-empty-detail ()
	(each (# (. %0 :sub)) (. *detail_container* :children))
	(bind '(sw &) (. *detail_scroll* :get_size))
	(bind '(vsw &) (if (get :vslider *detail_scroll*) (. (get :vslider *detail_scroll*) :get_constraint) '(16 0)))
	(defq page_w (max 420 (- sw vsw 16)))
	(def (defq md (Md))
		:page_width page_w
		:zoom 1.0
		:base_font_size 14)
	(. *detail_container* :add_child md)
	(defq lines (list
		"# Hacker News Live Feed"
		""
		"Welcome to the ChrysaLisp **Hacker News Reader**!"
		""
		"---"
		""
		"### Features"
		"* Live stories across **Top**, **Newest**, **Show HN**, **Ask HN**, and **Jobs**."
		"* Click any story on the left to read article information and discussion threads."
		"* Formatted text powered by ChrysaLisp's native `(Md)` widget."
		""
		"Select a story on the left to begin reading."))
	(. md :populate_lines lines)
	(bind '(w h) (. *detail_container* :pref_size))
	(. *detail_container* :change_dirty 0 0 (max w page_w) h :t)
	(.-> *detail_scroll* :layout :dirty_all))

(defun trigger-fetch-item (item_id worker_mbox)
	(def (. *status_label* :dirty) :text (cat "Loading #" (str item_id) "..."))
	(defq url (cat "http://node-hnapi.herokuapp.com/item/" (str item_id)))
	(defq task_code (str `(progn
		(import "service/net/app.inc")
		(import "lib/net/http.inc")
		(import "lib/net/json.inc")
		(ensure-net-service)
		(defq result :nil)
		(catch
			(progn
				(defq resp (http-get ,url))
				(when resp
					(defq body (http-body-str resp))
					(when (and body (starts-with "{" (trim body)))
						(defq json (json-parse body))
						(when json
							(defq raw_comments (or (pfind json :comments) (list))
								comment_list (list))
							(each (lambda (c)
								(push comment_list (list
									(or (pfind c :user) "anon")
									(or (pfind c :time_ago) "")
									(or (pfind c :content) ""))))
								raw_comments)
							(defq post_content (or (pfind json :content) ""))
							(setq result (list :item ,item_id post_content comment_list))))))
			(progn (setq result :nil) :t))
		(mail-send (hex-decode ,(hex-encode worker_mbox)) (str result)))))
	(open-child task_code +kn_call_run))

(defun render-loading-stories ()
	(each (# (. %0 :sub)) (. *story_container* :children))
	(defq lbl (Label))
	(def lbl :text "Fetching stories..." :font +font_btn :border 6 :ink_color +argb_grey8)
	(. *story_container* :add_child lbl)
	(bind '(w h) (. *story_container* :pref_size))
	(bind '(sw &) (. *story_scroll* :get_size))
	(bind '(vsw &) (if (get :vslider *story_scroll*) (. (get :vslider *story_scroll*) :get_constraint) '(16 0)))
	(defq cw (max w (max 320 (- sw vsw))))
	(. *story_container* :change_dirty 0 0 cw (max h 40) :t)
	(.-> *story_scroll* :layout :dirty_all))

(defun render-story-list ()
	(each (# (. %0 :sub)) (. *story_container* :children))
	(defq sel_id (if *selected_story* (first *selected_story*) *selected_id*))
	(each (lambda (story)
		(defq idx (!))
		(bind '(id title points user time_ago comments_count url domain) story)
		(defq is_selected (= id sel_id)
			card_flow (Flow)
			title_btn (Button)
			meta_flow (Flow)
			score_lbl (Label)
			meta_lbl (Label))
		(def card_flow :flow_flags +flow_down_fill :border 1)
		(def title_btn
			:text (cat (str (inc idx)) ". " (truncate-title title 38))
			:font +font_btn
			:border (if is_selected 1 0))
		(when is_selected
			(def card_flow :color (canvas-brighter (get :color *window*)))
			(def title_btn :color 0xffff6600))
		(. title_btn :connect (+ +event_story_0 idx))
		(def meta_flow :flow_flags +flow_right_fill)
		(def score_lbl
			:text (cat (str points) " pts")
			:ink_color 0xffff6600
			:font +font_small
			:border 0)
		(def meta_lbl
			:text (cat " * " user " * " (str comments_count) " cmts")
			:ink_color +argb_grey8
			:font +font_small
			:border 0)
		(.-> meta_flow (:add_child score_lbl) (:add_child meta_lbl))
		(.-> card_flow (:add_child title_btn) (:add_child meta_flow))
		(. *story_container* :add_child card_flow))
		*current_stories*)
	(def (. *story_count_label* :dirty) :text (cat (str (length *current_stories*)) " stories"))
	(bind '(w h) (. *story_container* :pref_size))
	(bind '(sw &) (. *story_scroll* :get_size))
	(bind '(vsw &) (if (get :vslider *story_scroll*) (. (get :vslider *story_scroll*) :get_constraint) '(16 0)))
	(defq cw (max w (max 320 (- sw vsw))))
	(. *story_container* :change_dirty 0 0 cw h :t)
	(.-> *story_scroll* :layout :dirty_all))

(defun select-story (idx worker_mbox)
	(when (and (>= idx 0) (< idx (length *current_stories*)))
		(setq *selected_story* (elem-get *current_stories* idx))
		(bind '(id title points user time_ago comments_count url domain) *selected_story*)
		(setq *selected_id* id)
		(config-save)
		(def (. *item_info_label* :dirty) :text (cat "#" (str id) " | " domain))
		(render-story-list)
		(defq cached_comments (. *comments_cache* :find id)
			cached_content (. *post_content_cache* :find id))
		(if cached_comments
			(render-detail-pane *selected_story* cached_content cached_comments)
			(progn
				(render-detail-pane *selected_story* :nil :nil)
				(trigger-fetch-item id worker_mbox)))))

(defun trigger-fetch-feed (category worker_mbox)
	(def (. *status_label* :dirty) :text (cat "Updating " category "..."))
	(defq endpoint (case category
		("top" "news")
		("newest" "newest")
		("show" "show")
		("ask" "ask")
		("jobs" "jobs")
		(:t "news")))
	(defq url (cat "http://node-hnapi.herokuapp.com/" endpoint))
	(defq task_code (str `(progn
		(import "service/net/app.inc")
		(import "lib/net/http.inc")
		(import "lib/net/json.inc")
		(ensure-net-service)
		(defq result :nil)
		(catch
			(progn
				(defq resp (http-get ,url))
				(when resp
					(defq body (http-body-str resp))
					(when (and body (starts-with "[" (trim body)))
						(defq json (json-parse body))
						(when json
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
								(push story_list (list id title points user time_ago comments_count url_link domain)))
								json)
							(setq result (list :feed ,category story_list))))))
			(progn (setq result :nil) :t))
		(mail-send (hex-decode ,(hex-encode worker_mbox)) (str result)))))
	(open-child task_code +kn_call_run))

(defun select-category (cat_name worker_mbox)
	(setq *selected_category* cat_name)
	(config-save)
	(update-tab-buttons)
	(render-loading-stories)
	(trigger-fetch-feed cat_name worker_mbox))

(ui-window *window* (:color +argb_grey15)
	(ui-title-bar _ "Hacker News" (0xea19) +event_close)
	; Header Navigation Bar
	(ui-flow header_bar (:flow_flags +flow_right_fill :border 1)
		(ui-label _ (:text " HN " :color 0xffff6600 :ink_color +argb_white :font +font_title :border 1))
		(. (ui-button *btn_top* (:text "Top" :font +font_btn)) :connect +event_tab_top)
		(. (ui-button *btn_new* (:text "Newest" :font +font_btn)) :connect +event_tab_new)
		(. (ui-button *btn_show* (:text "Show HN" :font +font_btn)) :connect +event_tab_show)
		(. (ui-button *btn_ask* (:text "Ask HN" :font +font_btn)) :connect +event_tab_ask)
		(. (ui-button *btn_jobs* (:text "Jobs" :font +font_btn)) :connect +event_tab_jobs)
		(. (ui-button *btn_refresh* (:text "Refresh" :font +font_btn)) :connect +event_refresh)
		(ui-label *status_label* (:text "Connecting..." :font +font_small :border 0 :ink_color +argb_grey8)))
	; makes the main_split use up all the remaining space !
	(ui-flow _ (:flow_flags +flow_up_fill)
		; Footer Status Bar
		(ui-flow status_bar (:flow_flags +flow_right_fill :border 1)
			(ui-label _ (:min_width 4 :border 0))
			(ui-label *story_count_label* (:text "0 stories" :font +font_small :border 0 :min_width 80))
			(ui-label _ (:flow_flags +flow_right_fill :border 0))
			(ui-label *item_info_label* (:text "Select a story" :font +font_small :border 0 :ink_color +argb_grey8 :min_width 160))
			(ui-label _ (:min_width 6 :border 0)))
		; Split Pane Body: Story List (Left) + Detail/Md Viewer (Right)
		(ui-flow main_split (:flow_flags +flow_right_fill)
			(ui-scroll *story_scroll* +scroll_flag_vertical (:min_width 340 :min_height 460)
				(ui-flow *story_container* (:flow_flags +flow_down_fill)))
			(ui-scroll *detail_scroll* +scroll_flag_both (:min_width 500 :min_height 460)
					(ui-flow *detail_container* (:flow_flags +flow_down_fill))))))

(defun main ()
	(config-load)
	(defq select (task-mboxes +select_size) *running* :t
		refresh_interval (* 300 1000000)) ; 5 minutes
	(def *window* :tip_mbox (elem-get select +select_tip))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (.-> *window* (:change x y w h :t) :dirty_all))
	(update-tab-buttons)
	(render-loading-stories)
	(render-empty-detail)
	(trigger-fetch-feed *selected_category* (elem-get select +select_worker))
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
							(render-story-list)
							(if (and *current_stories* (nempty? *current_stories*))
								(select-story 0 (elem-get select +select_worker))))
						(:item
							(bind '(& item_id post_content comments) res)
							(. *comments_cache* :insert item_id comments)
							(. *post_content_cache* :insert item_id post_content)
							(def (. *status_label* :dirty) :text "Loaded discussion")
							(when (and *selected_story* (= (first *selected_story*) item_id))
								(render-detail-pane *selected_story* post_content comments))))
					(progn
						(def (. *status_label* :dirty) :text "Fetch error")
						(.-> *status_label* :layout :dirty))))
			(+select_timer
				(mail-timeout (elem-get select +select_timer) refresh_interval 0)
				(trigger-fetch-feed *selected_category* (elem-get select +select_worker)))
			(+select_tip
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			(+select_main
				(defq id (getf *msg* +ev_msg_target_id))
				(cond
					((= id +event_close)
						(setq *running* :nil))
					((= id +event_refresh)
						(trigger-fetch-feed *selected_category* (elem-get select +select_worker)))
					((= id +event_tab_top)
						(select-category "top" (elem-get select +select_worker)))
					((= id +event_tab_new)
						(select-category "newest" (elem-get select +select_worker)))
					((= id +event_tab_show)
						(select-category "show" (elem-get select +select_worker)))
					((= id +event_tab_ask)
						(select-category "ask" (elem-get select +select_worker)))
					((= id +event_tab_jobs)
						(select-category "jobs" (elem-get select +select_worker)))
					((and (>= id +event_story_0) (< id (+ +event_story_0 50)))
						(select-story (- id +event_story_0) (elem-get select +select_worker)))
					((. *window* :event *msg*))))))
	(config-save)
	(gui-sub-rpc *window*))
