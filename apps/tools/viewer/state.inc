;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(import "service/lock/app.inc")

(defq +config_version 1 +config_file (cat *env_home* +state_filename))

(defun config-save ()
	;save state
	(with-write-lock +config_file
		(when (defq stream (file-stream +config_file +file_open_write))
			(tree-save stream (scatter (Emap)
				:version +config_version :zoom (get :zoom *window*) :find (. *find_text* :get_text)
				:whole_words *whole_words* :regexp *regexp* :ignore_case *ignore_case*))
			(stream-flush stream)
			(setq stream :nil))))

(defun config-load ()
	;load state
	(defq config (with-read-lock +config_file
		(when (defq stream (file-stream +config_file))
			(defq cfg (tree-load stream) stream :nil)
			cfg)))
	(when (and config (= (. config :find :version) +config_version))
		(bind '(zoom find_txt wwords regx icase)
			(gather config :zoom :find :whole_words :regexp :ignore_case))
		(def *window* :zoom zoom)
		(. *find_text* :set_text find_txt)
		(setq *whole_words* wwords *regexp* regx *ignore_case* icase))
	(update-find-toolbar))
