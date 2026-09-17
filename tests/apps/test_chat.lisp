(import "usr/env.inc")
(import "././utils.inc")

(report-header "Chat App Markdown & History Tests")

(defq *config_file* (cat *env_home* "chat_test.tre") *config_version* 1
	*config* :nil *history* (list) *max_display* 5 *max_history* 10)

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:username "TestUser"
		:max_display *max_display*
		:history (list)))

(defun config-save (user history_list)
	(if (not *config*)
		(setq *config* (Emap)))
	(when (> (length history_list) *max_history*)
		(setq history_list (slice history_list (- (length history_list) *max_history*) -1)))
	(scatter *config*
		:version *config_version*
		:username user
		:max_display *max_display*
		:history history_list)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

(defun config-load ()
	(defq old_config :nil)
	(if (defq stream (file-stream *config_file*))
		(setq old_config (tree-load stream)))
	(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
		(setq *config* (config-default))
		(setq *config* old_config))
	(defq u (. *config* :find :username) h (. *config* :find :history))
	(list (if (str? u) u "Guest") (if (list? h) h (list))))

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

; Test 1: Packet parsing
(defq p1 (parse-chat-msg "<Alice> Hello world!\n"))
(assert-eq "sender Alice" "Alice" (first p1))
(assert-eq "body Hello world!" "Hello world!" (second p1))

(defq p2 (parse-chat-msg "<Bob> Has joined the chat !\n"))
(assert-eq "sender Bob" "Bob" (first p2))
(assert-eq "body joined" "Has joined the chat !" (second p2))

(defq p3 (parse-chat-msg "System notice"))
(assert-eq "sender System" "System" (first p3))
(assert-eq "body System notice" "System notice" (second p3))

; Test 2: Formatting join and leave notices
(defq f_join (format-chat-lines "Bob" "Has joined the chat !"))
(assert-true "join has bold name" (some (# (find "**Bob**" %0)) f_join))
(assert-true "join has italic notice" (some (# (find "*has joined the chat.*" %0)) f_join))
(assert-true "join has divider" (some (# (eql "---" %0)) f_join))

(defq f_leave (format-chat-lines "Bob" "Has left the chat !"))
(assert-true "leave has bold name" (some (# (find "**Bob**" %0)) f_leave))
(assert-true "leave has italic notice" (some (# (find "*has left the chat.*" %0)) f_leave))

; Test 3: Formatting regular markdown message
(defq f_msg (format-chat-lines "Alice" "Hello **world**!\n* Item 1\n* Item 2"))
(assert-eq "first line is sender" "**Alice**" (first f_msg))
(assert-true "contains bold text" (some (# (find "**world**" %0)) f_msg))
(assert-true "contains list item 1" (some (# (find "* Item 1" %0)) f_msg))
(assert-true "contains list item 2" (some (# (find "* Item 2" %0)) f_msg))
(assert-eq "last line is divider" "---" (last f_msg))

; Test 4: History bounding logic
(defq test_hist (list))
(each (lambda (i) (push test_hist (cat "<User" (str i) "> Msg " (str i)))) (range 0 16))
(assert-eq "initial list size" 16 (length test_hist))
(defq bounded (if (> (length test_hist) *max_history*)
	(slice test_hist (- (length test_hist) *max_history*) -1) test_hist))
(assert-eq "bounded to max_history" 10 (length bounded))
(assert-eq "last element preserved" "<User15> Msg 15" (last bounded))

; Test 5: Config persistence roundtrip
(config-save "ChrysaHacker" bounded)
(bind '(loaded_user loaded_hist) (config-load))
(assert-eq "loaded username" "ChrysaHacker" loaded_user)
(assert-eq "loaded history length" 10 (length loaded_hist))
(assert-eq "loaded history last item" "<User15> Msg 15" (last loaded_hist))

; Cleanup test config
(pii-remove *config_file*)

(print-summary)

(stream-flush (io-stream "stdout"))
(task-sleep 500000)
(pii-exit)
