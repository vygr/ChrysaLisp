;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

;what the app was set to when it was last closed, kept in the user's home,
;see docs/ai_digest/app_configuration.md: the learning rate and the batch
;chosen, the farm or the GPU, and the net, not where its window was

(import "service/lock/app.inc")

(defq *config* :nil +config_version 1 +config_file (cat (get :home *env*) "digits.tre"))

(defun config-default ()
	(scatter (Emap) :version +config_version :rate 1 :batch 1 :gpu :nil :net 0))

(defun config-load ()
	;the defaults with what was kept laid over them
	(setq *config* (config-default))
	(and (defq old (with-read-lock +config_file
				(tree-load (file-stream +config_file))))
		(<= (ifn (. old :find :version) 0) +config_version)
		(. old :each (lambda (key val)
			(unless (eql key :version) (. *config* :insert key val))))))

(defun config-apply ()
	;the rate, the batch, the GPU and the net as the config says, each one
	;of its bar
	(bind '(r b g n) (gather *config* :rate :batch :gpu :net))
	(setq rate (max 0 (min (dec (length +digits_rates)) r))
		batch (max 0 (min (dec (length +digits_batches)) b))
		which (max 0 (min (dec (length +digits_nets)) (ifn n 0))))
	(. *rate_bar* :set_selected rate)
	(. *batch_bar* :set_selected batch)
	(. *net_bar* :set_selected which)
	(setq gpu_mode (and has_gpu g :t))
	(. *mode_bar* :set_selected (if gpu_mode 1 0)))

(defun config-save ()
	(scatter *config* :rate rate :batch batch :gpu (and gpu_mode (not to_farm)) :net which)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))
