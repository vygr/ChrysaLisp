;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught persistent configuration & demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/lock/app.inc")

(defq +config_version 1
	+config_file (cat *env_home* "onslaught.tre")
	*config* :nil *best_demo_score* 0 *best_demo_data* :nil
	*session_best_score* 0 *session_best_demo* :nil
	*hiscore_names* :nil *hiscore_values* :nil
	+default_hiscore_names
	''("NEGROG (CAH)" "RUDMAN (CAH)" "GORVAR (CAH)" "TREGAR (CAH)"
		"TATGO  (CAH)" "GONIG  (CAH)" "INDIGO (CAH)" "MOLAT  (CAH)"
		"DENGAR (CAH)" "LOGRAA (CAH)")
	+default_hiscore_values
	''(10000 9000 8000 7000 6000 5000 4000 3000 2000 1000))

(defun config-default ()
	(scatter (Emap)
		:version +config_version
		:difficulty 0
		:mode +mode_auto
		:sound 0
		:hiscore_names +default_hiscore_names
		:hiscore_values +default_hiscore_values
		:demo_score 0
		:demo ""))

(defun config-load ()
	(setq *config* (with-read-lock +config_file
		(tree-load (file-stream +config_file))))
	(if (or (nil? *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (config-default)))
	(setq *menu_difficulty* (. *config* :find :difficulty)
		*menu_mode* (. *config* :find :mode)
		*game_mode* *menu_mode*
		*menu_sound* (. *config* :find :sound)
		*hiscore_names* (. *config* :find :hiscore_names)
		*hiscore_values* (. *config* :find :hiscore_values)
		*best_demo_score* (. *config* :find :demo_score)
		*best_demo_data* (. *config* :find :demo)
		*session_best_score* *best_demo_score*
		*session_best_demo* *best_demo_data*))

(defun config-save ()
	(when *session_best_demo*
		(setq *best_demo_score* *session_best_score*
			*best_demo_data* *session_best_demo*))
	(scatter *config*
		:difficulty *menu_difficulty*
		:mode *menu_mode*
		:sound *menu_sound*
		:hiscore_names *hiscore_names*
		:hiscore_values *hiscore_values*
		:demo_score *best_demo_score*
		:demo (or *best_demo_data* ""))
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))
