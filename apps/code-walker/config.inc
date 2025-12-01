;;;;;;;;;;;;;;;;;;;;;;
; apps/code-walker/config.inc
;;;;;;;;;;;;;;;;;;;;;;

(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "code-walker.tre"))

(defun config-default ()
	; Create default configuration with all settings
	(scatter (Emap)
		:version *config_version*
		; Toggle states
		:show_diffs :t
		:show_tree :nil
		:compare_mode :nil
		; History
		:history (list)
		:history_index -1
		; Current inputs
		:input_a ""
		:input_b ""
		; Current outputs (optional - could be large)
		:output_read ""
		:output_expand ""
		:output_bind ""
		:output_eval ""))

(defun config-update ()
	; Update config with current application state
	(scatter *config*
		; Save toggle states
		:show_diffs *show_diffs*
		:show_tree *show_tree*
		:compare_mode *compare_mode*
		; Save history
		:history *history*
		:history_index *history_index*
		; Save current inputs
		:input_a (get :text *input_field*)
		:input_b (get :text *input_field2*)
		; Save current outputs
		:output_read (get :text *read_output*)
		:output_expand (get :text *expand_output*)
		:output_bind (get :text *bind_output*)
		:output_eval (get :text *eval_output*)))

(defun config-save ()
	; Save configuration to .tre file
	(config-update)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)
		(close stream)))

(defun config-load ()
	; Load configuration from .tre file with migration support
	(defq old-config :nil)
	(when (defq stream (file-stream *config_file* +file_open_read))
		(setq old-config (tree-load stream))
		(close stream))

	; Check if migration is needed
	(if (or (not old-config) (< (. old-config :find :version 0) *config_version*))
		(progn
			; Start with defaults
			(setq *config* (config-default))
			; Migrate old values if they exist
			(when old-config
				(. old-config :each (lambda (key val)
					(unless (eql key :version)
						(. *config* :insert key val)))))
			; Ensure version is current
			(. *config* :insert :version *config_version*))
		; Version matches, use loaded config
		(setq *config* old-config)))

(defun config-restore ()
	; Restore application state from loaded config
	; Called after config-load during startup
	(when *config*
		; Restore toggle states
		(setq *show_diffs* (. *config* :find :show_diffs :t))
		(setq *show_tree* (. *config* :find :show_tree :nil))
		(setq *compare_mode* (. *config* :find :compare_mode :nil))

		; Restore history
		(setq *history* (. *config* :find :history (list)))
		(setq *history_index* (. *config* :find :history_index -1))

		; Restore inputs
		(set *input_field* :text (. *config* :find :input_a ""))
		(set *input_field2* :text (. *config* :find :input_b ""))

		; Restore outputs
		(set *read_output* :text (. *config* :find :output_read ""))
		(set *expand_output* :text (. *config* :find :output_expand ""))
		(set *bind_output* :text (. *config* :find :output_bind ""))
		(set *eval_output* :text (. *config* :find :output_eval ""))

		; Update UI to match restored state
		(set *diff_toggle_btn* :text (if *show_diffs* "Diffs: ON" "Diffs: OFF"))
		(set *tree_toggle_btn* :text (if *show_tree* "Tree: ON" "Tree: OFF"))
		(set *compare_toggle_btn* :text (if *compare_mode* "Compare: ON" "Compare: OFF"))
		(def *input2_section* :visible *compare_mode*)
		(update-history-label)

		; Mark layouts as dirty
		(.-> *diff_toggle_btn* :layout :dirty)
		(.-> *tree_toggle_btn* :layout :dirty)
		(.-> *compare_toggle_btn* :layout :dirty)
		(.-> *input_field* :layout :dirty)
		(.-> *input_field2* :layout :dirty)
		(.-> *input2_section* :layout :dirty)
		(.-> *read_output* :layout :dirty)
		(.-> *expand_output* :layout :dirty)
		(.-> *bind_output* :layout :dirty)
		(.-> *eval_output* :layout :dirty)))
