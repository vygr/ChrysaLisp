;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught demo mode recording & playback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	; input recording state for demo mode
	*record_stream* :nil
	*record_keys* -1
	*record_count* 0
	*record_start_score* 0
	*demo_skip_armed* :nil)

(defun record-battle-start ()
	; initialize demo recording stream with initial battle state header
	(setq *record_keys* -1
		*record_count* 0
		*record_start_score* *man_score*
		*record_stream* (memory-stream))
	(defq header (list *game_level* *enemy_army* *enemy_banner* *man_banner* *enemy_wizshot*
		*man_power* *man_strength* *item_selected* *item_inventory* *item_usage*
		*menu_difficulty* *menu_mode* *man_score*))
	(write-line *record_stream* (str header)))

(defun record-battle-tick (k)
	; record one tick of user controls using 2-byte rle (keys, count)
	(when *record_stream*
		(defq ctrl (logand k 0xff))
		(if (= *record_keys* -1)
			(setq *record_keys* ctrl *record_count* 0)
			(if (or (/= *record_keys* ctrl) (= *record_count* 255))
				(progn
					(write-byte *record_stream* *record_keys*)
					(write-byte *record_stream* *record_count*)
					(setq *record_keys* ctrl *record_count* 0))
				(++ *record_count*)))))

(defun record-battle-finish ()
	; finish recording and if battle score beats current best demo score, update and save to config
	(when *record_stream*
		(when (/= *record_keys* -1)
			(write-byte *record_stream* *record_keys*)
			(write-byte *record_stream* *record_count*))
		(stream-seek *record_stream* 0 0)
		(defq len (stream-length *record_stream*)
			demo_str (read-blk *record_stream* len)
			battle_score (- *man_score* *record_start_score*))
		(setq *record_stream* :nil)
		(when (> battle_score *best_demo_score*)
			(setq *best_demo_score* battle_score
				*best_demo_data* demo_str)
			(config-save))))

(defun demo-state-init ()
	(setq *demo_skip_armed* :nil))

(defun demo-state-update ()
	(if (/= *game_controls* 0)
		(setq *demo_skip_armed* :t)
		(when *demo_skip_armed*
			(setq *demo_skip_armed* :nil)
			(goto-title +game_state_menu))))
