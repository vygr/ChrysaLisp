;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(defun save-state ()
	;save editor state
	(when (defq stream (file-stream (cat *env_home* +state_filename) +file_open_write))
		(maptree-save stream *meta_map* '(":nil" :buffer :nil))))

(defun load-state ()
	;load editor state
	(setq *meta_map* (Fmap-kv :files (Fmap)) *open_files* (list))
	(when (defq stream (file-stream (cat *env_home* +state_filename)))
		(setq *meta_map* (maptree-load stream))
		(bind '(file find_txt replace_txt files)
			(gather *meta_map* :file :find :replace :files))
		(. *find_text* :set_text (id-decode find_txt))
		(. *replace_text* :set_text (id-decode replace_txt))
		(defq dead_files (list))
		(.-> files (:resize 101) (:each (lambda (file meta)
			(cond
				((= (age file) 0)
					;no such file
					(push dead_files file))
				(:t (bind '(cx cy ax ay sx sy fx fy fx1 fy1)
						(gather meta :cx :cy :ax :ay :sx :sy :fx :fy :fx1 :fy1))
					(push *open_files* file)
					(. meta :insert :buffer :nil)
					(populate-buffer file cx cy ax ay sx sy fx fy fx1 fy1))))))
		(each (# (. files :erase %0)) dead_files)
		(if (find file *open_files*) file)))
