;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

;what the app was set to when it was last closed, kept in the user's home,
;see docs/ai_digest/app_configuration.md: the learning rate and the batch
;chosen, not where its window was

(import "service/lock/app.inc")

(defq *config* :nil +config_version 1 +config_file (cat (get :home *env*) "mnist.tre"))

(defun config-default ()
	(scatter (Emap) :version +config_version :rate 1 :batch 1))

(defun config-load ()
	;the defaults with what was kept laid over them
	(setq *config* (config-default))
	(and (defq old (with-read-lock +config_file
				(tree-load (file-stream +config_file))))
		(<= (ifn (. old :find :version) 0) +config_version)
		(. old :each (lambda (key val)
			(unless (eql key :version) (. *config* :insert key val))))))

(defun config-apply ()
	;the rate and the batch as the config says, each one of its bar
	(bind '(r b) (gather *config* :rate :batch))
	(setq rate (max 0 (min (dec (length +mnist_rates)) r))
		batch (max 0 (min (dec (length +mnist_batches)) b)))
	(. *rate_bar* :set_selected rate)
	(. *batch_bar* :set_selected batch))

(defun config-save ()
	(scatter *config* :rate rate :batch batch)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))
