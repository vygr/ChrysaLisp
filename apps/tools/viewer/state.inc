;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(import "service/lock/app.inc")

(defq +state_filename "viewer.tre" *config_version* 1)

(defun config-save ()
	;save viewer state
	(defq key (const (cat *env_home* +state_filename)))
	(lock-claim-rpc key)
	(when (defq stream (file-stream key +file_open_write))
		(tree-save stream (scatter (Emap)
			:version *config_version*
			:zoom (get :zoom *window*))))
	(lock-release-rpc key))

(defun config-load ()
	;load viewer state
	(defq key (const (cat *env_home* +state_filename))
		old_config :nil)
	(lock-claim-rpc key)
	(when (defq stream (file-stream key))
		(catch (setq old_config (tree-load stream))
			(progn (setq old_config :nil) :t)))
	(lock-release-rpc key)
	(if (and old_config (= (. old_config :find :version) *config_version*))
		(if (defq zoom (. old_config :find :zoom))
			(def *window* :zoom zoom))))
