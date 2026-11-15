;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

;what the app was showing when it was last closed, kept in the user's home,
;see docs/ai_digest/app_configuration.md. Only the state of the app, not
;where its window was

(import "service/lock/app.inc")

(defq *config* :nil +config_version 1 +config_file (cat (get :home *env*) "template.tre"))

(defun config-default ()
	;the first time: the main view, and each button its theme's colour, :nil
	(scatter (Emap) :version +config_version :tab 0 :colors (list :nil :nil :nil :nil)))

(defun config-load ()
	;the defaults with what was kept laid over them, so a key added since it
	;was kept has its default, and one no longer used is passed over. A file
	;of a later version than this, or none, gives the defaults
	(setq *config* (config-default))
	(and (defq old (with-read-lock +config_file
				(tree-load (file-stream +config_file))))
		(<= (ifn (. old :find :version) 0) +config_version)
		(. old :each (lambda (key val)
			(unless (eql key :version) (. *config* :insert key val))))))

(defun config-apply ()
	;the app as the config says, the tab shown and the buttons' colours
	(bind '(tab colors) (gather *config* :tab :colors))
	(. *stack_flow* :show_tab tab)
	(each (# (if %1 (def (. %0 :dirty) :paper %1)))
		(list *b1* *b2* *b3* *b4*) colors))

(defun config-update ()
	;the config as the app is, the tab shown and the buttons' colours, a
	;colour set on the button itself, not one it has from what it is in
	(scatter *config*
		:tab (. (. *stack_flow* :get_radio_bar) :get_selected)
		:colors (map (# (def? :paper %0)) (list *b1* *b2* *b3* *b4*))))

(defun config-save ()
	(config-update)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))
