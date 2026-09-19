;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(import "service/lock/app.inc")

(defq *config_version* 1)

(defun config-save ()
	;save state
	(defq key (cat *env_home* +state_filename))
	(lock-claim-rpc key)
	(when (defq stream (file-stream key +file_open_write))
		(tree-save stream (scatter (Emap)
			:version *config_version*
			:zoom (get :zoom *window*)
			:find (. *find_text* :get_text)
			:whole_words *whole_words*
			:regexp *regexp*
			:ignore_case *ignore_case*)))
	(lock-release-rpc key))

(defun config-load ()
	;load state
	(defq key (cat *env_home* +state_filename)
		old_config :nil)
	(lock-claim-rpc key)
	(when (defq stream (file-stream key))
		(catch (setq old_config (tree-load stream))
			(progn (setq old_config :nil) :t)))
	(lock-release-rpc key)
	(when (and old_config (= (. old_config :find :version) *config_version*))
		(bind '(zoom find_txt wwords regx icase)
			(gather old_config :zoom :find :whole_words :regexp :ignore_case))
		(if zoom (def *window* :zoom zoom))
		(if find_txt (. *find_text* :set_text find_txt))
		(if wwords (setq *whole_words* wwords))
		(if regx (setq *regexp* regx))
		(if icase (setq *ignore_case* icase)))
	(update-find-toolbar))
