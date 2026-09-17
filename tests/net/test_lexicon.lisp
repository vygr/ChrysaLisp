(import "usr/env.inc")
(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "lib/net/json.inc")
(import "././utils.inc")

(report-header "Lexicon Dictionary & Thesaurus Tests")

(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "lexicon_test.tre")
	*history* (list) *last_word* "lisp")

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:last_word "lisp"
		:history '("lisp" "computer" "algorithm")))

(defun config-save (last_w hist)
	(if (not *config*)
		(setq *config* (Emap)))
	(scatter *config*
		:version *config_version*
		:last_word last_w
		:history hist)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

(defun config-load ()
	(defq old_config :nil)
	(if (defq stream (file-stream *config_file*))
		(setq old_config (tree-load stream)))
	(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
		(setq *config* (config-default))
		(setq *config* old_config))
	(setq *last_word* (. *config* :find :last_word))
	(if (not (str? *last_word*)) (setq *last_word* "lisp"))
	(setq *history* (. *config* :find :history))
	(if (not (list? *history*)) (setq *history* (list))))

(defun pos-label (tag)
	(case tag
		("n" "Noun")
		("v" "Verb")
		("adj" "Adjective")
		("adv" "Adverb")
		("u" "Interjection")
		(:t (upper (str tag)))))

(defun format-word-markdown (word phonetic defs syns ants)
	(defq lines (list (cat "# " (to-lower word)) ""))
	(when (and phonetic (nempty? (trim phonetic)))
		(push lines (cat "*Pronunciation:* `/" (trim phonetic) "/`") ""))
	(push lines "---" "")
	(if (empty? defs)
		(push lines "*No definitions found for this word.*" "")
		(progn
			(push lines "### Definitions" "")
			(defq count 1)
			(each (lambda ((pos_tag def_text))
				(push lines (cat (str count) ". **[" (pos-label pos_tag) "]** " def_text) "")
				(setq count (inc count)))
				defs)))
	(when (nempty? syns)
		(push lines "---" "" "### Synonyms" ""
			(cat (join syns ", ") ".") ""))
	(when (nempty? ants)
		(push lines "---" "" "### Antonyms" ""
			(cat (join ants ", ") ".") ""))
	lines)

; Test 1: Config roundtrip
(config-save "recursion" '("recursion" "fractal" "lisp"))
(setq *last_word* :nil *history* :nil *config* :nil)
(config-load)
(assert-eq "config roundtrip last word" "recursion" *last_word*)
(assert-eq "config roundtrip history length" 3 (length *history*))
(assert-eq "config roundtrip first history" "recursion" (first *history*))

; Test 2: Part-of-speech label mapping
(assert-eq "pos noun" "Noun" (pos-label "n"))
(assert-eq "pos verb" "Verb" (pos-label "v"))
(assert-eq "pos adj" "Adjective" (pos-label "adj"))

; Test 3: Markdown formatter
(defq md_lines (format-word-markdown "test" "t eh s t"
	'(("n" "A trial or examination.") ("v" "To challenge."))
	'("exam" "trial") '("pass")))
(assert-true "md has title" (some (# (starts-with "# test" %0)) md_lines))
(assert-true "md has noun def" (some (# (find "[Noun]" %0)) md_lines))
(assert-true "md has synonyms" (some (# (find "exam" %0)) md_lines))

; Test 4: Live network fetch for definition from Datamuse
(ensure-net-service)
(defq url "http://api.datamuse.com/words?sp=lisp&md=dpr&max=1"
	resp (catch (http-get url (pmap :connection "close")) :nil))
(assert-true "datamuse http response" (if resp :t :nil))
(when resp
	(defq body (http-body-str resp))
	(assert-true "datamuse body non-empty" (and body (nempty? (trim body))))
	(assert-true "datamuse body is json list" (starts-with "[" (trim body)))
	(when (starts-with "[" (trim body))
		(defq json (catch (json-parse body) :nil))
		(assert-true "datamuse json parsed" (list? json))
		(assert-true "datamuse word list non-empty" (nempty? json))
		(when (nempty? json)
			(defq entry (first json)
				entry_word (pfind entry :word)
				entry_defs (or (pfind entry :defs) (list)))
			(assert-eq "datamuse word is lisp" "lisp" entry_word)
			(assert-true "datamuse has definitions" (nempty? entry_defs)))))

; Test 5: Live network fetch for synonyms
(defq syn_url "http://api.datamuse.com/words?rel_syn=fast&max=5"
	syn_resp (catch (http-get syn_url (pmap :user-agent "curl/8.7.1")) :nil))
(assert-true "synonym http response" (if syn_resp :t :nil))

(when syn_resp
	(defq syn_body (http-body-str syn_resp))
	(when (and syn_body (starts-with "[" (trim syn_body)))
		(defq syn_json (catch (json-parse syn_body) :nil))
		(assert-true "synonyms parsed list" (list? syn_json))
		(assert-true "synonyms non-empty" (nempty? syn_json))))

(print-summary)

(stream-flush (io-stream "stdout"))
(task-sleep 500000)
(pii-exit)
