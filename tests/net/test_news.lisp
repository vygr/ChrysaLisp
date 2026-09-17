(import "usr/env.inc")
(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "lib/net/json.inc")
(import "lib/text/searching.inc")
(import "././utils.inc")

(report-header "Hacker News Feed & Reader Tests")

(defq
	*config* :nil
	*config_version* 1
	*config_file* (cat *env_home* "news_test.tre")
	*selected_category* "top")

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:selected_category "top"
		:selected_id 0))

(defun config-save (cat id)
	(if (not *config*)
		(setq *config* (Emap)))
	(scatter *config*
		:version *config_version*
		:selected_category cat
		:selected_id id)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

(defun config-load ()
	(defq old_config :nil)
	(if (defq stream (file-stream *config_file*))
		(setq old_config (tree-load stream)))
	(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
		(setq *config* (config-default))
		(setq *config* old_config))
	(setq *selected_category* (. *config* :find :selected_category))
	(if (not (str? *selected_category*))
		(setq *selected_category* "top")))

(defun clean-hn-text (text)
	(if (not (str? text))
		""
		(replace-regex
			(reduce! (lambda (txt pat rep) (replace-str txt pat rep))
				'(("<p>" "</p>" "<pre><code>" "</code></pre>" "<code>" "</code>" "<i>" "</i>" "<b>" "</b>"
					"&#x27;" "&#39;" "&#x2F;" "&quot;" "&lt;" "&gt;" "&amp;")
				("\n\n" "" "\n```\n" "\n```\n" "`" "`" "*" "*" "**" "**"
					"'" "'" "/" "\q" "<" ">" "&")) text)
			"<[^>]+>" "")))

; Test 1: Config roundtrip
(config-save "show" 49740047)
(setq *selected_category* :nil *config* :nil)
(config-load)
(assert-eq "config roundtrip category" "show" *selected_category*)
(assert-eq "config roundtrip id" 49740047 (. *config* :find :selected_id))

; Test 2: Text cleaning
(defq raw_sample "I&#x27;ve tested <code>foo()</code> &amp; &quot;bar&quot;.<p>Next paragraph with link.")
(defq cleaned (clean-hn-text raw_sample))
(assert-true "cleaned decodes entities" (found? cleaned "I've tested `foo()` & \qbar\q."))
(assert-true "cleaned formats p" (found? cleaned "\n\nNext paragraph with link."))

; Test 3: Live HTTP fetch of top stories
(ensure-net-service)
(defq resp (http-get "http://node-hnapi.herokuapp.com/news"))
(assert-true "http response received" (if resp :t :nil))

(when resp
	(defq body (http-body-str resp))
	(assert-true "http body non-empty" (and body (> (length body) 0)))
	(defq json (json-parse body))
	(assert-true "json parse successful" (if json :t :nil))
	(when json
		(assert-true "news items non-empty" (> (length json) 0))
		(defq item (first json))
		(assert-true "item has title" (str? (pfind item :title)))
		(assert-true "item has id" (num? (pfind item :id)))))

(print-summary)

(stream-flush (io-stream "stdout"))
(task-sleep 500000)
(pii-exit)
